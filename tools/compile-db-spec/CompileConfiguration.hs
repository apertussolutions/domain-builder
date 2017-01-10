{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative ((<$), (<$>), Alternative(..), Applicative(..))
import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.Char (toLower, isDigit)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Serialize.Put (runPut)
import Data.Traversable (for, traverse)
import Data.Word (Word64)
import Numeric (readHex)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (many1, letter, parse, string, space, spaces, char, eof, between, optional)
import qualified Data.ByteString as B
import qualified Text.Parsec as P
import qualified Text.Parsec as P

import DeviceModelSettings
import ShellDmRender
import SimpleKeyValue
import Spec
import BinDbSpec(saveSpec)
import qualified StaticCheckMonad as Check

usageString =
  do prog <- getProgName
     return $ prog ++ " INPUT_FILENAME DB_OUTPUT_FILENAME [DM_OUTPUT_FILENAME]"

main = do
  (input, dboutput, dmoutput) <- getPathArgs
  txt     <- readFile input
  kv <- case parse parseFile input txt of
          Left e -> failure (show e)
          Right (File kv []) -> return kv
          Right (File _ others) -> failure $ unlines
              ("Unexpected anonymous values:" : map show others)

  let (res, m) = Check.run kv $ (,) <$> processDB <*> processDM

  reportUnusedKeys m

  case res of
    Left errs -> failure $ unlines $ "Configuration failed because:" : map (' ':) errs

    Right (dbcfg, dmcfg) ->
      do saveSpec dboutput dbcfg
         for_ dmoutput $ \ fn -> writeFile fn (renderForShell dmcfg)

getPathArgs =
  do xs <- getArgs
     case xs of
       [a,b,c] -> return (a,b,Just c)
       [a,b] -> return (a,b,Nothing)
       _ -> failure =<< usageString


reportUnusedKeys us = unless (null us)
                   $ hPutStrLn stderr $ "Unused keys: " ++ intercalate ", " us

-----------------------------------------------------------
-- Error handling logic -----------------------------------
-----------------------------------------------------------

failure str =
  do hPutStrLn stderr str
     exitFailure

-----------------------------------------------------------
-- Parser implementations ---------------------------------
-----------------------------------------------------------

type CheckM = Check.M String Value

processDB :: CheckM Spec
processDB = Spec
        <$> getMb "domid"
        <*> getMb "vcpus" `defaultTo` 1
        <*> (mbToKb <$> get "memory")
        <*> getMaxmem
        <*> getDomainFlags "dom_flags"
        <*> get "kernel"
        <*> getMb "ramdisk"
        <*> processCommandLine
        <*> getMb "kernel_hash"
        <*> getMb "ramdisk_hash"
        <*> getMb "access_control"
        <*> getVMSpec

processCommandLine :: CheckM String
processCommandLine = merge <$> getMb "root" <*> getMb "extra"
  where
  merge root extra = intercalate " " $ catMaybes [("root="++) <$> root, extra]

processDM :: CheckM DMSettings
processDM = DMSettings
        <$> getVBDs "disk"
        <*> getVIFs "vif"
        <*> getMb "device_model"
        <*> getIsHVM
        <*> getMb "sdl" `defaultTo` False
        <*> getMb "tpm" `defaultTo` False
        <*> getMb "vtpm" `defaultTo` False
        <*> getMb "tpmmgr" `defaultTo` False
        <*> getMb "opengl" `defaultTo` False
        <*> getVNCSettings
        <*> getMb "stdvga" `defaultTo` False
        <*> getMb "serial"
        <*> getMb "name"
        <*> getMb "builder"
        <*> getMb "boot" `defaultTo` "c"
        <*> getVideoram

getMaxmem = do
  mb <- getMb "maxmem"
  mbToKb <$> case mb of
    Just x -> return x
    Nothing -> do isHVM <- getIsHVM
                  if isHVM then (+) <$> get "memory" <*> getVideoram
                           else get "memory"


getVNCSettings = do
  vnc <- getMb "vnc" `defaultTo` False
  if vnc
    then fmap Just $
         VNCSettings
           <$> getMb "vncunused" `defaultTo` True
           <*> getMb "vnclisten"
           <*> getMb "vncdisplay"
           <*> getMb "vncpasswd"
           <*> getMb "vncconsole" `defaultTo` False
    else pure Nothing

getVideoram = getMb "videoram" `defaultTo` 4
getIsHVM = getMb "hvm" `defaultTo` False

getVMSpec = do
  isHVM <- getIsHVM
  if isHVM then HVM <$> getHVMSpec
           else PV  <$> getPVSpec

getHVMSpec = pure HVMSpec

getPVSpec = PVSpec
        <$> getBitmap "irqs"
        <*> getRanges "iomem"
        <*> getRanges "ioports"


getBitmap key = do
  ns <- getMb key `defaultTo` [] :: CheckM [Word64]
  return $ map (`elem` ns) [0..maximum (0:ns)]


getDomainFlags :: String -> CheckM Word64
getDomainFlags key = parseDomFlags =<< getMb key `defaultTo` []

parseDomFlags flags = foldr (.|.) 0 <$> mapM (parseDomFlag . lowercase) flags

parseDomFlag "privileged"    = return (2^0)
parseDomFlag "initdomain"    = return (2^1)
parseDomFlag "multiboot_mod" = return (2^2)
parseDomFlag xs = fail ("Unknown domain flag: " ++ show xs)



getRanges key = do
  xs <- getMb key `defaultTo` []
  for xs $ \ s -> case parse parseRange (key ++ " entry: " ++ s) s of
            Left err -> Check.except (show err)
            Right rs -> return rs

parseRange = do
  spaces
  enabled <- command
  res <- numberRange enabled <|> allRange enabled
  eof
  return res

  where
  command = False <$ string "disable" <* space <* spaces
        <|> True  <$ optional (string "enable" <* space <* spaces)

  symbol c = char c *> spaces

  numberRange enabled = do
    start   <- fromInteger <$> parseNumber
    rangeEnd start enabled <|> rangeSize start enabled <|> singleRange start enabled

  rangeEnd start enabled =
   do symbol '-'
      end     <- fromInteger <$> parseNumber
      unless (start < end) (fail $ show start ++ " is not less than " ++ show end)
      return $ Range { start = start, how_many = end - start + 1, enabled = enabled}

  rangeSize start enabled = between (symbol '(') (symbol ')') $
   do size     <- fromInteger <$> parseNumber
      unless (0 < size) (fail "Size must be greater than zero")
      return $ Range { start = start, how_many = size, enabled = enabled}

  singleRange start enabled =
   return $ Range { start = start, how_many = 1, enabled = enabled}

  allRange enabled = do
    string "all"
    spaces
    return $ Range { start = 0, how_many = -1, enabled = enabled}

-----------------------------------------------------------
-- VIF string parser --------------------------------------
-----------------------------------------------------------

getVIFs key = do
  xs <- getMb key `defaultTo` []
  for xs $ \ s -> case parse parseVIF (key ++ " entry: " ++ s) s of
            Left err -> Check.except (show err)
            Right rs -> return rs

parseVIF :: P.Parsec String () VIF
parseVIF = parseVifAux emptyVif
  
parseVifAux vif = do
   key <- P.many1 letter
   char '='
   val <- many1 (P.satisfy (\x -> x /= ',' && x /= ' '))

   let vifAdd f v
         | isNothing (f vif) = continue v
         | otherwise         = fail ("Duplicate key " ++ show key)

       continue v = (char ',' >> parseVifAux v)
                <|> (eof >> return v)

   case key of
     "type"    -> vifAdd vifType    vif { vifType    = Just val }
     "model"   -> vifAdd vifModel   vif { vifModel   = Just val }
     "name"    -> vifAdd vifName    vif { vifName    = Just val }
     "mac"     -> vifAdd vifMac     vif { vifMac     = Just val }
     "bridge"  -> vifAdd vifBridge  vif { vifBridge  = Just val }
     "ip"      -> vifAdd vifIP      vif { vifIP      = Just val }
     "script"  -> vifAdd vifScript  vif { vifScript  = Just val }
     "backend" | all isDigit val -> vifAdd vifBackend vif { vifBackend = Just (read val) }
               | otherwise -> fail "Bad backend domain id"
     _         -> fail ("Unknown key " ++ show key)

-----------------------------------------------------------
-- VBD string parser --------------------------------------
-----------------------------------------------------------

getVBDs key = do
  xs <- getMb key `defaultTo` []
  for xs $ \ s -> case parse parseVBD (key ++ " entry: " ++ s) s of
            Left err -> Check.except (show err)
            Right rs -> return rs

parseVBD :: P.Parsec String () VBD
parseVBD = do
  spaces
  dev <- parseDiskDevice
  char ','
  spaces
  (vdev,vtype) <- parseVirtualDiskDevice
  char ','
  spaces
  mode <- parseDiskMode
  -- We don't support alternate domain ids yet
  eof
  return $ VBD
    { vbdDeviceType = dev
    , vbdDev        = vdev
    , vbdMode       = mode
    , vbdType       = vtype
    }
  

parseDiskDevice = parseFileDevice

parseFileDevice = do
  P.try (string "file:")
  path <- many (P.noneOf ",")
  return (FileDevice path)

parseVirtualDiskDevice = do
  vdev <- P.manyTill P.anyChar (char ':')
  vtype <- parseVType
  return (vdev,vtype)

parseVType = P.try (Disk  <$ string "disk")
         <|> P.try (Cdrom <$ string "cdrom")

parseDiskMode = ReadOnlyDeviceMode  <$ char 'r'
            <|> ReadWriteDeviceMode <$ char 'w'

-----------------------------------------------------------
-- Typed key lookup function ------------------------------
-----------------------------------------------------------

requiredCheck key Nothing = Check.except $ key ++ " is required"
requiredCheck key (Just x) = return x

class ValueCast a where
  cast :: String -> Value -> CheckM a

instance ValueCast Word64 where
  cast _ (Atom (Number n)) = return (fromInteger n)
  cast key _ = Check.except (key ++ " must be a number")

instance ValueCast String where
  cast _ (Atom (String s)) = return s
  cast key _ = Check.except (key ++ " must be a string")

instance ValueCast [String] where
  cast key (Atom _) = Check.except $ key ++ " must be a list of strings"
  cast key (List xs) = for xs $ \ x ->
        case x of
          Number _ -> Check.except $ key ++ " must be a list of strings"
          String s -> return s  

instance ValueCast [Word64] where
  cast key (Atom _) = Check.except $ key ++ " must be a list of numbers"
  cast key (List xs) = for xs $ \ x ->
        case x of
          String _ -> Check.except $ key ++ " must be a list of numbers"
          Number n -> return (fromInteger n)


instance ValueCast Bool where
  cast _ (Atom (String "yes")) = pure True
  cast _ (Atom (String "no"))  = pure False
  cast _ (Atom (Number 0))     = pure False
  cast _ (Atom (Number _))     = pure True
  cast key _    = Check.except (key ++ " must be 'yes' or 'no'")

getMb key = traverse (cast key) =<< Check.mbGetKey key
get   key = requiredCheck key =<< getMb key

-----------------------------------------------------------
-- Utilities ----------------------------------------------
-----------------------------------------------------------

defaultTo :: Functor f => f (Maybe a) -> a -> f a
defaultTo m x = fromMaybe x <$> m

mbToKb n = 1024*n

lowercase = map toLower
