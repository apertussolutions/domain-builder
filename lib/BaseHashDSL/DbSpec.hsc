{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}

module DbSpec where

import Control.Monad (unless,when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S (pack)
import Data.Word
import Foreign.Ptr(Ptr,plusPtr)
import Foreign.Marshal.Array
import Foreign.Storable(peekByteOff)
import System.IO.MMap (mmapWithFilePtr, Mode(..))

#include <db-spec.h>

data DomFlag = IsPrivileged
 deriving (Show)
type DomId = Word64

data DbConfig = DbConfig
  { domId               :: Maybe DomId
  , maxVcpus            :: Word64
  , allocateKbs         :: Word64
  , maxKbs              :: Word64
  , domFlags            :: Word64
  , console             :: Maybe DomId
  , xenstore            :: Maybe DomId
  , isHvm               :: Bool
  , commandLine         :: String
  , securityContext     :: String
  , kernelName          :: String
  , ramdiskName         :: Maybe String
  , kernelHash          :: Maybe ByteString
  , ramdiskHash         :: Maybe ByteString
  }
 deriving (Show)

parseConfig :: FilePath -> IO DbConfig
parseConfig file = mmapWithFilePtr file ReadOnly Nothing $ \(ptr,size) ->
  do let size1 = (#size db_config_bin) :: Int
     when (size < size1) $ fail "Invalid configuarion file (1)"
     string_size <- (#peek db_config_bin, strings_size) ptr
     unless (size == string_size + size1)
       $ fail "Invalid configuration file (2)"
     peekConfig ptr




peekConfig :: Ptr () -> IO DbConfig
peekConfig ptr = do
    domid        <- (#peek db_config_bin, domid              ) ptr :: IO Word64
    max_vcpus    <- (#peek db_config_bin, max_vcpus          ) ptr :: IO Word64
    allocate_kbs <- (#peek db_config_bin, allocate_kbs       ) ptr :: IO Word64
    max_kbs      <- (#peek db_config_bin, allocate_kbs       ) ptr :: IO Word64
    dom_flags    <- (#peek db_config_bin, dom_flags          ) ptr :: IO Word64
    console      <- (#peek db_config_bin, console            ) ptr :: IO Word64
    xenstore     <- (#peek db_config_bin, xenstore           ) ptr :: IO Word64
    is_hvm       <- (#peek db_config_bin, is_hvm             ) ptr :: IO Word64

    command_line <- (#peek db_config_bin, command_line_offset) ptr :: IO Word64
    security_ctx <- (#peek db_config_bin, security_context_offset)ptr :: IO Word64
    kernel_name  <- (#peek db_config_bin, kernel_name_offset ) ptr :: IO Word64
    ramdisk_name <- (#peek db_config_bin, ramdisk_name_offset) ptr :: IO Word64
    kernel_hash  <- (#peek db_config_bin, kernel_hash_offset ) ptr :: IO Word64
    ramdisk_hash <- (#peek db_config_bin, ramdisk_hash_offset) ptr :: IO Word64
    strings_size <- (#peek db_config_bin, strings_size       ) ptr :: IO Word64
    let strings_ptr = ptr `plusPtr` (#offset db_config_bin, strings)
    strings0      <- peekArray (fromIntegral strings_size) strings_ptr :: IO [Word8]
    let strings = map (toEnum . fromEnum) strings0

        optString off
          | off >= strings_size = Nothing
          | otherwise           = Just $ takeWhile (/= '\0')
                                       $ drop (fromIntegral off) strings

        string off = takeWhile (/= '\0') $ drop (fromIntegral off) strings

        hash off
          | off >= strings_size = Nothing
          | otherwise           = Just $ S.pack $ take 32
                                       $ drop (fromIntegral off) strings0


    return DbConfig
      { domId           = if domid == 0 then Nothing else Just domid
      , maxVcpus        = max_vcpus
      , allocateKbs     = allocate_kbs
      , domFlags        = dom_flags
      , console         = if console == (#const DB_CONFIG_NONE) then Nothing else Just console
      , xenstore        = if xenstore == (#const DB_CONFIG_NONE) then Nothing else Just xenstore
      , isHvm           = is_hvm /= 0
      , maxKbs          = max_kbs
      , commandLine     = string command_line
      , securityContext = string security_ctx
      , kernelName      = string kernel_name
      , ramdiskName     = optString ramdisk_name
      , kernelHash      = hash kernel_hash
      , ramdiskHash     = hash ramdisk_hash
      }
