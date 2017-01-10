{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BaseHashDSL
  ( -- * Basic Types and Concepts
    Stmt
  , compile
  , compileWithStamp

  -- * Hashing Sections
  , hash
  , onlyPadding

  -- ** SectionSpec modifiers
  , SectionSpec
  , section
  , offset
  , noPad

  -- * Locations and Addresses
  , Location(..)
  , locAddr

  -- ** Regions
  , Region, regionStart, regionLength, regionEnd

  -- ** Interesting Memory Regions
  , getPMPRegion, getKernelRegion, getVMRegion, getStartInfoRegion
  , getPageTablesRegion, getRamdiskRegion

  -- * Memory Sizes
  , getPhysMemPages
  , getPageTables

  -- * Unmapped Pages
  , unmapped

  -- * Zero Areas
  , zeroBetween
  , zeroBetweenRegions

  -- * Custom Expected Values
  --
  -- | Occasionally, the content of a region of memory is not present in the
  -- object file explicitly but we have knowledge about its content
  -- at a measurement point.  The functions in this Section provide a way
  -- for describing such expectations.

  , expect
  , expectStruct
  , expectStructOfType
  , expectStartInfo

  -- ** Low-level Memroy Specs

  , expectMem
  , MemSpec
  , sizeOf

  -- | Currently, we represent multi-byte values in little endian format.
  , uint8_t, uint16_t, uint32_t, uint64_t

  -- ** Combining Memory Specs
  , struct, skip

  -- * Example
  -- $examples
  ) where

import VM
import Control.Applicative (Applicative)
import Control.Monad (when, zipWithM_, forM_)
import Data.Digest.Pure.SHA (showDigest, sha256)
import Data.Elf
import qualified Data.Sequence as Seq
import Data.Bits ((.&.), shiftR)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (find, insertBy, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (for)
import Data.Serialize ( runPut, runGet, remaining, Get, Put
                      , getWord8, getWord16le, getWord32le, getWord64le
                      , putWord8, putWord16le, putWord32le, putWord64le
                      , putLazyByteString
                      )
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.IO.MMap (mmapFileByteString)
import System.Exit(exitFailure)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import MonadLib ( ReaderT, StateT, Id
                , liftM2
                , ask, sets_, get, set
                , runReaderT, runStateT, runId
                , unless
                )
import Data.Dwarf.C.Elf(parseElfBinary)
import Data.Dwarf.C

import DbSpec

-- The DSL ---------------------------------------------------------------------


-- | A program in the language consists of a sequence of /statements/.
-- Multiple statements may be sequenced using Haskell's 'do' construct:
--
-- > do statement1
-- >    statement2
-- >    statement3
--
-- Each statement has a type of the form @Stmt r@,  where @r@ is the
-- type of the value computed by the statement.  Statements
-- that do not have a return value are of type @Stmt ()@.
--
-- The notation for naming the value computed by a statement is:
--
-- > do page_num <- getPhysMemPages
-- >    statement1
-- >    statement2
--
-- In this example, the statement @getPhysMemPages@ computes the number
-- of physical pages to be allocated for the virtual machine and names
-- the result @page_num@.  The value @page_num@ can be used in
-- subsequent statements.

newtype Stmt a = Stmt (DebugReader (ReaderT SpecInfo (StateT RawSpec Id)) a)
                  deriving (Functor, Monad, Applicative)


data SpecInfo = SpecInfo
  { theElfFile   :: Elf
  , theSymTab    :: SymbolTable
  , dbConfig     :: Maybe DbConfig
  , ramdisk      :: Maybe S.ByteString
  }


-- | A number of statements in the language need to refer to locations
-- in the address space of the virtual machine which is being measured.
-- The type 'Location' provides different ways to refer addresses.
data Location
  = At      Word64           -- ^ An explicit numeric address.
  | Global  String           -- ^ The address of a global symbol.
  | InFile  String String    -- ^ The address of a symbol defined in
                             --   the given file.


-- | Get the symbol table associated with the ELF file.
-- The symbol table is a function mapping symbol names to their address.
-- Currently, the program is aborted if we ask for the address of a symbol
-- which is not present in the symbol table.

symbolTable :: Stmt (String -> Word64)
symbolTable = Stmt (theSymTab `fmap` ask)

-- | Compute the address of a location.
locAddr :: Location -> Stmt Word64
locAddr loc =
  case loc of
    At x          -> return x
    Global x      -> lkp (lookupVar x) x
    InFile file x -> lkp (lookupVarIn file x) x
  where
  -- Fall back on the symbol table
  lkp m x = do mb <- Stmt (tryDebug m)
               case mb of
                 Right a -> return (addrOf a)
                 Left _ ->
                   -- No debug info, try symbol table
                   do tab <- symbolTable
                      return (tab x)

rawSpec ::  (RawSpec -> RawSpec) -> Stmt ()
rawSpec = Stmt . sets_

-- | Specify that we should not automatically pad a section with 0.
noPad :: SectionSpec -> SectionSpec
noPad s = s { sectPad = False }

-- | Specify that we should skip the given number of bytes
-- when hashing the section.  Multiple calls to offset accumulate.
offset :: SectionSpec     -- ^ Spec which is being modified.
       -> Word64          -- ^ Offset from the beginning of the section.
       -> SectionSpec
offset s o = s { sectOffset = sectOffset s + o }

-- | Specification which hashes an entire section.
-- Sections of type NOBITS are filled with 0.
-- The hash for the section will be padded with 0 up to a page boundary.
section :: String         -- ^ Name of section
        -> SectionSpec
section name = Section { sectName = name, sectOffset = 0, sectPad = True }


-- | Include the digest described by the given section specification
-- in the hash.
hash :: SectionSpec
     -> Stmt ()
hash sect = rawSpec $ \spec -> spec { sections = sections spec Seq.|> sect }


-- | This statement asserts that the expected hash should include zero-padding
-- after the given section but should not include the section content.
-- The padding extends to the following page boundary.
-- The statement is useful for sections whose content is not known
-- statically but we'd like to know that no data \"appeared\"
-- in-between sections.
onlyPadding :: String     -- ^ Section name
            -> Stmt ()
onlyPadding name =
  do elf <- Stmt (theElfFile `fmap` ask)
     let size = S.length $ elfSectionData $ findSection name elf
     hash (section name `offset` fromIntegral size)

-- | Declare that a page of memory is expected to be unmapped at run-time.
-- The address should be page aligned.
unmapped :: Location -> Stmt ()
unmapped loc =
  do addr <- locAddr loc
     insertException addr Nothing


-- | A description of the values we expect to occupy some part of memory.
data MemSpec
  = MemSkip Word64
  | MemExpect S.ByteString
  | MemJn MemSpec MemSpec

-- | Compute the number of bytes spanned by the given memory specification.
sizeOf :: MemSpec -> Word64
sizeOf (MemJn x y)    = sizeOf x + sizeOf y
sizeOf (MemSkip n)    = n
sizeOf (MemExpect bs) = fromIntegral (S.length bs)

memSpec :: (a -> Put) -> a -> MemSpec
memSpec f a = MemExpect (runPut (f a))

expectBytes :: [(Word64, L.ByteString)] -> Stmt ()
expectBytes = mapM_ $ \(addr,bytes) -> insertException addr (Just bytes)

insertException :: Word64 -> Maybe L.ByteString -> Stmt ()
insertException addr mbBs = rawSpec $ \ spec ->
  spec { exceptions = insertBy (comparing fst) (addr, mbBs) (exceptions spec) }

byteSpec :: MemSpec -> Word64 -> [(Word64,L.ByteString)] ->
                                      ([(Word64, L.ByteString)], Word64)
byteSpec spec start zs =
  case spec of
    MemSkip _     -> (zs, start + sizeOf spec)
    MemExpect bs  -> ((start, L.fromChunks [bs]) : zs, start + sizeOf spec)
    MemJn x y     -> let (xs,start1) = byteSpec x start ys
                         (ys,end)    = byteSpec y start1 zs
                     in (xs,end)

-- | Provide custom values to be expected at the given location.
expectMem :: Location  -- ^ Where to expect the specific values.
          -> MemSpec   -- ^ What values to expect at the given location.
          -> Stmt ()
expectMem loc mem =
  do addr <- locAddr loc
     expectBytes $ fst $ byteSpec mem addr []


-- | A sequence of adjacent memory specifications.
-- This is similar to a (packed) struct in C.
struct :: [MemSpec] -> MemSpec
struct [] = MemSkip 0
struct xs = foldr1 MemJn xs

skip :: Word64 -> MemSpec
skip = MemSkip


cTypeMemSpec :: Type -> Word64 -> Stmt MemSpec
cTypeMemSpec ty v = Stmt $
  do Size n <- sizeOfType ty
     -- The reason why we don't insist that this is a primitive
     -- type is because we also use it for word-sized structs with bitfields.
     case n of
       1 -> return $ uint8_t  (fromIntegral v)
       2 -> return $ uint16_t (fromIntegral v)
       4 -> return $ uint32_t (fromIntegral v)
       8 -> return $ uint64_t (fromIntegral v)
       _ -> do txt <- formatTypeName ty
               error ("Invalid primitive type: " ++ show txt)

cStructSpec :: Word64 -> Type -> [(String, Word64)] -> Stmt ()
cStructSpec addr ty fSpecs =
  do fs <- Stmt (flattenFields ty)
     forM_ fSpecs $ \(f,w) ->
       case [ (o,t) | (n,Offset o,t) <- fs, n == f ] of
         (o,t) : _ -> expectMem (At (addr + o)) =<< cTypeMemSpec t w
         _ -> do txt <- Stmt (formatTypeName ty)
                 error ("Type " ++ show txt ++ " has no field " ++ show f)


expectCVar :: Word64 -> GlobalVarInfo -> Stmt ()
expectCVar val info =
  do spec <- cTypeMemSpec (globalVarType info) val
     let Addr addr = globalVarAddr info
     expectMem (At addr) spec

getGlobal :: String -> Stmt GlobalVarInfo
getGlobal x = Stmt (lookupVar x)

getGlobalIn :: FilePath -> String -> Stmt GlobalVarInfo
getGlobalIn file x = Stmt (lookupVarIn file x)

addrOf :: GlobalVarInfo -> Word64
addrOf i = let Addr x = globalVarAddr i in x

-- | Specify the expected value of a C variable.
-- We lookup the type of the C variable from the debugging
-- information and set the expectation using the appropriate type.
--
-- If this function is called in an explicit address, then
-- the expectation is just the given 64-bit value.
expect :: Location -> Word64 -> Stmt ()
expect loc@(At _) v       = expectMem loc (uint64_t v)
expect (Global x) v       = expectCVar v =<< getGlobal x
expect (InFile file x) v  = expectCVar v =<< getGlobalIn file x

-- | Specify expected values for the fields of a struct.
-- The type of the fields is determined based on the type of the struct,
-- which is obtained from debugging information.
expectStruct :: Location            -- ^ Location of a struct value.
             -> [(String, Word64)]  -- ^ Values for the fields.
             -> Stmt ()
expectStruct loc fs =
  case loc of
    At _ -> error "expectStruct called with an address---cannot determine type"
    Global x      -> spec =<< getGlobal x
    InFile file x -> spec =<< getGlobalIn file x
  where spec info = cStructSpec (addrOf info) (globalVarType info) fs

-- | Specify expected values for the fields of a struct.
-- The type of the fields is determined based on the type of the struct,
-- which is provided in the second argument.
expectStructOfType :: Location            -- ^ Location of a struct value.
                   -> String              -- ^ Typedef name for struct type.
                   -> [(String, Word64)]  -- ^ Values for the struct fields.
                   -> Stmt ()
expectStructOfType loc ty fs =
  do t <- getTypeDef ty
     addr <- locAddr loc
     cStructSpec addr t fs


getTypeDef :: String -> Stmt Type
getTypeDef name = Stmt (lookupTypeDef name)

-- | Provide expected values for fields in the start-info page of a
-- virtual machine.
expectStartInfo :: [(String,Word64)] -> Stmt ()
expectStartInfo fs =
  do addr <- getStartInfoPageAddr
     expectStructOfType (At addr) "start_info_t" fs


uint8_t :: Word8 -> MemSpec
uint8_t = memSpec putWord8

uint16_t :: Word16 -> MemSpec
uint16_t = memSpec putWord16le

uint32_t :: Word32 -> MemSpec
uint32_t = memSpec putWord32le

uint64_t :: Word64 -> MemSpec
uint64_t = memSpec putWord64le




getRamdiskAddr :: Stmt Word64
getRamdiskAddr =
  do elf <- Stmt (theElfFile `fmap` ask)
     return (roundPageUp (kernelEnd elf))


getRamdiskSize :: Stmt Word64
getRamdiskSize =
  do mb <- Stmt (ramdisk `fmap` ask)
     case mb of
       Just bs -> return $ fromIntegral $ S.length bs
       Nothing ->
         do mbSpec <- Stmt (dbConfig `fmap` ask)
            case mbSpec of
              Nothing -> return 0     -- Assuming no RAM disk...
              Just spec ->
                case ramdiskName spec of
                  Nothing -> return 0
                  Just _  -> error "Spec needs a RAM disk"


-- XXX: Assuming no special note for this.
getPMPAddr :: Stmt Word64
getPMPAddr =
  do ra <- getRamdiskAddr
     rs <- getRamdiskSize
     return (roundPageUp (ra + rs))

getPMPSize :: Stmt (Word64, Word64)
getPMPSize =
  do elf <- Stmt (theElfFile `fmap` ask)
     mbc <- Stmt (dbConfig `fmap` ask)
     case mbc of
       Just spec -> return (pmpSize elf spec)
       Nothing   -> error "Spec needs the VM configuration"

getStartInfoPageAddr :: Stmt Word64
getStartInfoPageAddr =
  do pa <- getPMPAddr
     (pages,entry) <- getPMPSize
     return (roundPageUp (pa + pages * entry))

getPageTablesAddr :: Stmt Word64
getPageTablesAddr =
  do sa <- getStartInfoPageAddr
     return (sa + 3 * pageSize)   -- start info, xenstore, console


getPageTablesInfo :: Stmt (Word64, Word64)
getPageTablesInfo =
  do elf <- Stmt (theElfFile `fmap` ask)
     end <- getPageTablesAddr
     return (ptabNum (kernelStart elf) (end - 1))



ptabNum :: Word64 -> Word64 -> (Word64, Word64)
ptabNum start end = loop 4
  where
  lastAddr x = div (x + (0x400000 - 1)) 0x400000 * 0x400000 - 1

  loop ptabs = let newEnd = lastAddr (end + pageSize * ptabs + 512 * 1024)
                   ptabs1 = ptabNumForRegion start newEnd
              in if ptabs == ptabs1 then (ptabs, newEnd) else loop ptabs1


ptabNumForRegion :: Word64 -> Word64 -> Word64
ptabNumForRegion start end = sum (scanl next 1 [ 3, 2, 1 ])
  where lvl n x     = (x `shiftR` (12 + n * 9)) .&. 511
        next prev n = 512 * (prev - 1) + lvl n end - lvl n start + 1



-- | Embed a DSL program in Haskell.  Typically, this function
-- would be used only once, in the definition of the function @main@.
-- Usually, a DSL program looks like this:
--
-- > import BaseHashDSL
-- >
-- > main = compile $
-- >  do statement1
-- >     statement2
-- >     statement3
--
-- The resulting Haskell program can be used to compute base-line
-- measurements from an ELF file.  It has the following command line
-- arguments:
--
-- @ usage: ELF_FILE OUT_FILE
--  -r filename  --ramdisk=filename  RAM disk
--  -s filename  --spec=filename     Domain-builder specification
-- @
--
-- The first argument is the name of an ELF file which should be
-- read to compute the expected hashes.  The output is stored in
-- the second file, which contains instructions for the measurer on
-- how to compute the expected hash.  The file has the following format
-- (multi-byte values are little endian):
--
-- @ struct {
--    uint64_t entry_num;
--    entry_t entires[entry_num];
-- } file_format_t;
--
-- typedef struct {
--   uint64_t region_start;
--   uint64_t region_lenght;
-- } entry_t;
-- @
--
-- The measurer should hash together each memory region, in the order
-- in which they appear in the file.
--
-- Regions of 0 length have a special meaning:  they indicate that the
-- measurer should expect an unmapped page at the given address.
-- Such regions are not included in the measured hash.
--

compile :: Stmt () -> IO ()
compile s = compileMaybeStamp Nothing [s]

compileWithStamp :: Location -> [Stmt ()] -> IO ()
compileWithStamp l s = compileMaybeStamp (Just l) s

compileMaybeStamp :: Maybe Location -> [Stmt ()] -> IO ()
compileMaybeStamp mb specs =
  do opts <- measureOpts
     elf <- elfFile (kernelFile opts)
     cfg <- for (dbConfigFile opts) parseConfig
     rd  <- for (ramdiskFile  opts) (\file -> mmapFileByteString file Nothing)
     let one m = do reset
                    _ <- m
                    s <- Stmt get
                    return (specStream elf s)

     let (stmp, regs) = runStmt elf cfg rd
                           $ do ss <- mapM one specs
                                st <- case mb of
                                        Nothing -> return Nothing
                                        Just l  -> Just `fmap` locAddr l
                                return (st,ss)

     let (streams, regionss) = unzip regs
     mapM_ (putStrLn . showDigest . sha256) streams

     zipWithM_ (\n r -> putStrLn ("--- " ++ show n ++ " ---") >> explainRegions r) [1::Integer .. ] regionss

     S.writeFile (outputFile opts) (renderRegions stmp regionss)

explainRegions :: [RegionCommand] -> IO ()
explainRegions regions =
     zipWithM_ printRegionPair (HashRegion 0 0 : regions) regions
  where
  printRegionPair before x = do
    let start = regionCommandStart x
    let end   = regionCommandEnd before
    let len   = start - end
    when (len > 0) $ putStrLn $
      showString "skip:\t" $ sh end ++ " +" ++ sh len
    putStrLn (pp x)

  pp (HashRegion x y) = "hash:\t" ++ sh x ++ " +" ++ sh y
  pp (UnmappedPage x) = "unmap:\t" ++ sh x ++ " +" ++ sh (0x1000 :: Int)

  sh x = let a = showHex x ""
         in "0x" ++ replicate (16 - length a) '0' ++ a


--------------------------------------------------------------------------------


-- $examples
-- > import BaseHashDSL
-- >
-- > main :: IO ()
-- > main = compile $
-- >   do unmapped (At 0)
-- >
-- >      hash (section "static_code" `offset` 0x1000)
-- >      hash (section "static_data")
-- >      hash (section "reset_data")
-- >      onlyPadding "noreset_data"
-- >      hash (section "zero_reset")
-- >
-- >      expect (InFile "db-mm.c" "prev_chunk") (-1)


--------------------------------------------------------------------------------

pageSize :: Num a => a
pageSize = 4096

roundPageUp :: (Integral a, Num a) => a -> a
roundPageUp x = div (x + (pageSize - 1)) pageSize * pageSize


data RawSpec = RawSpec
  { sections   :: Seq.Seq SectionSpec
  , exceptions :: [ (Word64, Maybe L.ByteString) ]
  } deriving Show


-- | Often, we hash the content of a section based on the data in the
-- ELF file.  A 'SectionSpec' is a description of exactly how this
-- should be done.  In the simplest case, we just hash the entire section
-- but, occasionally, it is useful to customize this a bit.  For
-- example, we can specify if sections should be 0 padded to the next
-- page boundary, and offsets in the section where hashing should
-- begin.
data SectionSpec  = Section { sectName          :: String
                            , sectOffset        :: Word64
                            , sectPad           :: Bool
                            }
                      deriving Show

runStmt :: Elf -> Maybe DbConfig -> Maybe S.ByteString -> Stmt a -> a
runStmt elf dbc rd (Stmt m) =
  case val of
    (Left err, _) -> error err
    (Right a,  _) -> a

  where raw  = RawSpec Seq.empty []
        info = SpecInfo
                 { theElfFile = elf
                 , theSymTab = makeSymbolTable elf (makeStringTable elf)
                 , dbConfig  = dbc
                 , ramdisk   = rd
                 }
        bin = getDwarfInfo (parseElfBinary elf)
        val = runId $ runStateT raw $ runReaderT info $ runDebugReader bin m


reset :: Stmt ()
reset = Stmt (set (RawSpec Seq.empty []))

{-
defineSpec :: Elf -> Maybe DbConfig -> Maybe S.ByteString -> Stmt () -> RawSpec
defineSpec elf dbc rd (Stmt m) =
  case val of
    (Left err, _) -> error err
    (Right _,  s) -> s

  where raw  = RawSpec Seq.empty [] Nothing
        info = SpecInfo
                 { theElfFile = elf
                 , theSymTab = makeSymbolTable elf (makeStringTable elf)
                 , dbConfig  = dbc
                 , ramdisk   = rd
                 }
        bin = getDwarfInfo (parseElfBinary elf)
        val = runId $ runStateT raw $ runReaderT info $ runDebugReader bin m
-}


specStream :: Elf -> RawSpec -> (L.ByteString, [RegionCommand])
specStream elf raw
  | null errs = (hashInput vm1, vmToRegionList vm1)
  | otherwise = error (unlines errs)
  where
  (vm0, errs) = foldr addException (newVM, []) (exceptions raw)
  vm1 = compressVM $ foldr (addSection elf) vm0 (toList $ sections raw)

addException :: (Word64, Maybe L.ByteString)
             -> (VM, [String]) -> (VM, [String])
addException (addr, mbBs) (vm, errs) = (vm1, errs ++ errs1)
  where
  (vm1, errs1) = insertStrict addr expect1 vm
  expect1 = case mbBs of
              Nothing -> ExpectUnmapped 1
              Just bs -> ExpectBytes bs

addSection :: Elf -> SectionSpec -> VM -> VM
addSection elf spec = pad . insertOverlap offsetAddr offsetBytes
  where
  sect = findSection (sectName spec) elf
  size = elfSectionSize sect
  end  = elfSectionAddr sect + size
  rawbytes =
    case elfSectionType sect of
      SHT_PROGBITS -> L.fromChunks [elfSectionData sect]
      SHT_NOBITS   -> L.replicate (fromIntegral (elfSectionSize sect)) 0
      t            -> error ("Section " ++ sectName spec
                                        ++ " has unsupported type: " ++ show t)
  offsetBytes = L.drop (fromIntegral (sectOffset spec)) rawbytes
  offsetAddr = elfSectionAddr sect + sectOffset spec

  padlen = fromIntegral $ (pageSize - end) `mod` pageSize

  pad | sectPad spec && padlen /= 0 = insertOverlap end (L.replicate padlen 0)
      | otherwise    = id




-- Binary format:
--   8 bytes for the stamp address (-1, if none)
--   8 bytes of region count
--   regions
--     8 bytes of address
--     8 bytes of length
--
--     0 length regions represent unmapped pages
--
--    All values are little-endian.


renderRegions :: Maybe Word64 -> [[RegionCommand]] -> S.ByteString
renderRegions stmp regionss = runPut $ do
  putWord64le (fromMaybe (-1) stmp)
  putWord64le (fromIntegral (length regionss))
  forM_ regionss $ \regions ->
    do putWord64le (fromIntegral (length regions))
       mapM_ putRegionCommand regions

putRegionCommand :: RegionCommand -> Put
putRegionCommand (HashRegion _ 0)      = error "putRegionCommand: Zero length HashRegion"
putRegionCommand (HashRegion addr len) = putWord64le addr >> putWord64le len
putRegionCommand (UnmappedPage addr)   = putWord64le addr >> putWord64le 0



-- Working with ELF Files

type StringTable = Int -> String      -- ^ Maps offsets to strings
type SymbolTable = String -> Word64   -- ^ Maps symbols to addresses

elfFile :: FilePath -> IO Elf
elfFile file = parseElf `fmap` mmapFileByteString file Nothing


findSection :: String -> Elf -> ElfSection
findSection name elf = case find theOne (elfSections elf) of
                         Nothing -> error ("Missing section: " ++ name)
                         Just s  -> s
  where theOne sect = elfSectionName sect == name


-- XXX: We should consult VIRT_BASE & PADDR_OFFSET notes.
kernelStart :: Elf -> Word64
kernelStart = minimum . (0:) . map elfSegmentVirtAddr . elfSegments

-- XXX: We should consult VIRT_BASE & PADDR_OFFSET notes.
kernelEnd :: Elf -> Word64
kernelEnd = maximum . (0:) . map segmentEnd . elfSegments
  where segmentEnd s = elfSegmentVirtAddr s + elfSegmentMemSize s



makeStringTable :: Elf -> StringTable
makeStringTable elf i
  | i < 0 || S.length bytes <= fromIntegral i = error "no such symbol"
  | otherwise = S8.unpack
              $ S.takeWhile (/= 0)
              $ S.drop (fromIntegral i) bytes

  where bytes = elfSectionData (findSection ".strtab" elf)


makeSymbolTable :: Elf -> StringTable -> SymbolTable
makeSymbolTable elf strtab =
  case runGet (untilEmpty getSymbol) bytes of
    Left e -> error ("bad symbol table: " ++ e)
    Right xs -> \x -> fromMaybe (error $ "unknown symbol " ++ x) (lookup x xs)

  where
  bytes = elfSectionData (findSection ".symtab" elf)
  getSymbol = do
    name <- (strtab . fromIntegral) `fmap` getWord32le
    _    <- getWord8 >> getWord8 >> getWord16le
    value <- getWord64le
    _size <- getWord64le
    return (name,value)


-- | Return the number of bytes needed for the PMP given a configuration and elf
pmpSize :: Elf -> DbConfig -> (Word64, Word64)
pmpSize elf cfg = (pages, entrySize)
  where
  pages = (allocateKbs cfg * 1024 + pageSize - 1) `div` pageSize
  entrySize = case elfMachine elf of
    EM_X86_64 -> 8
    EM_386    -> 4
    unsupported -> error ("Unsupported machine type: " ++ show unsupported)


-- == Misc ==

untilEmpty :: Get a -> Get [a]
untilEmpty p = do
  n <- remaining
  if n == 0 then return [] else liftM2 (:) p (untilEmpty p)




-- == Configuration ==

measureOpts :: IO Config
measureOpts =
  do (flags, files, errors) <- getOpt RequireOrder options `fmap` getArgs
     unless (null errors) $ mapM_ (hPutStrLn stderr) errors >> nogo
     case files of
       [kernel, outFile] -> return $ makeOptions kernel outFile flags
       _                 -> nogo
  where
  nogo = do hPutStrLn stderr (usageInfo "usage: ELF_FILE OUT_FILE" options)
            exitFailure


options :: [OptDescr ConfigFlag]
options =
  [ Option ['r'] ["ramdisk"] (ReqArg RamdiskFlag "filename")
    "RAM disk"

  , Option ['s'] ["spec"] (ReqArg DbConfigFlag "filename")
    "Domain-builder specification"
  ]


data ConfigFlag   = RamdiskFlag FilePath
                  | DbConfigFlag FilePath

makeOptions :: FilePath -> FilePath -> [ConfigFlag] -> Config
makeOptions kernel outFile = foldr add (defaultConfig kernel outFile)
  where
  add (RamdiskFlag fp) opts  = opts { ramdiskFile = Just fp }
  add (DbConfigFlag fp) opts = opts { dbConfigFile = Just fp }



data Config = Config
  { kernelFile    :: FilePath
  , outputFile    :: FilePath
  , ramdiskFile   :: Maybe FilePath
  , dbConfigFile  :: Maybe FilePath
  }

defaultConfig :: FilePath -> FilePath -> Config
defaultConfig kernel outFile = Config
  { kernelFile = kernel
  , outputFile = outFile
  , ramdiskFile = Nothing
  , dbConfigFile = Nothing
  }


-- == ----------------------------------


data RegionCommand
  = HashRegion Word64 Word64 -- Hash this address for this many bytes
  | UnmappedPage Word64     -- This page is unmapped

regionCommandStart, regionCommandEnd :: RegionCommand -> Word64
regionCommandStart (HashRegion start _) = start
regionCommandStart (UnmappedPage start) = start
regionCommandEnd (HashRegion start len) = start + len
regionCommandEnd (UnmappedPage start) = start + pageSize

instance Show RegionCommand where
  showsPrec p (HashRegion addr len) = showParen (p > 10)
                                    $ showString "HashRegion 0x"
                                    . showHex    addr
                                    . showString " 0x"
                                    . showHex    len
  showsPrec p (UnmappedPage addr)   = showParen (p > 10)
                                    $ showString "UnmappedPage 0x"
                                    . showHex    addr

hashInput :: VM -> L.ByteString
hashInput (VM vm) = L.fromChunks $ (:[]) $ runPut $ mapM_ aux vm
  where
  aux (_   , ExpectUnmapped _) = return ()
  aux (addr, ExpectBytes bs) = do
    let startingLength = distanceToNextBoundary pageSize (fromIntegral addr)
    let (xs,ys)        = L.splitAt startingLength bs
    let pages          = xs : chunks pageSize ys
    mapM_ leadingZero pages

  leadingZero bs = do
    let (xs,ys) = L.span (==0) bs
    let n       = L.length xs
    putWord16le (fromIntegral n)
    putLazyByteString ys

-- | 'chunks' divides a 'ByteString' into a list of @n@ sized substrings.
-- The last element of this list might have fewer than @n@ bytes.
chunks :: Int64 -> L.ByteString -> [L.ByteString] 
chunks n = unfoldr aux
  where
  aux bs
    | L.null bs = Nothing
    | otherwise = Just (L.splitAt n bs)

-- | 'distanceToNextBoundary' returns the non-zero number which when
-- added to @i@ will result in a multiple of @n@.
distanceToNextBoundary :: Integral a => a -> a -> a
distanceToNextBoundary n i = n - i `mod` n

vmToRegionList :: VM -> [RegionCommand]
vmToRegionList (VM vm) = expectToRegionCommands =<< vm
  where
  expectToRegionCommands (addr, ExpectBytes bs) =
    [HashRegion addr (fromIntegral (L.length bs))]
  expectToRegionCommands (addr, ExpectUnmapped n) =
    [UnmappedPage (addr + a * pageSize) | a <- [0 .. n-1]]


-- == Memory Regions ==

-- A contiguous region of memory.
data Region = Region Word64 Word64
  deriving (Eq)

instance Show Region where
  showsPrec p (Region start len) = showParen (p > 10)
                                 $ showString "Region 0x"
                                 . showHex start
                                 . showString " 0x"
                                 . showHex len


-- | First address in a region.
regionStart :: Region -> Word64
regionStart  (Region start _) = start

-- | Number of bytes in the region.
regionLength :: Region -> Word64
regionLength (Region _ len)   = len

-- | The address of the first byte immediately after the region.
regionEnd :: Region -> Word64
regionEnd region = regionStart region + regionLength region

-- | The number of physical pages allocated for the VM.
-- This information is based on the domain-builder spec.
getPhysMemPages :: Stmt Word64
getPhysMemPages = fst `fmap` getPMPSize

-- | The number of page tables allocated for the VM initially.
getPageTables :: Stmt Word64
getPageTables = fst `fmap` getPageTablesInfo


-- | The region of memory containing the pseudo-physical to machine address
-- mapping.  The region includes only meaningful content and no padding.
getPMPRegion :: Stmt Region
getPMPRegion = do
  start <- getPMPAddr
  (pages, entrySize) <- getPMPSize
  return (Region start (pages * entrySize))

-- | The region of memory containing the initial page tables for a VM.
-- The region includes only meaningful content and no padding.
getPageTablesRegion :: Stmt Region
getPageTablesRegion = do
  start <- getPageTablesAddr
  ptabs <- getPageTables
  return (Region start (ptabs * pageSize))

-- | The region of memory containing the RAM-disk for a VM.
-- The region includes only meaningful content and no padding.
getRamdiskRegion :: Stmt Region
getRamdiskRegion = do
  start <- getRamdiskAddr
  size  <- getRamdiskSize
  return (Region start size)

-- | The region of memory containing the initial kernel for the VM.
-- This encompasses all loadable segments of the ELF file.
-- The region includes only meaningful content and no padding.
getKernelRegion :: Stmt Region
getKernelRegion = do
  elf <- Stmt (theElfFile `fmap` ask)
  let start = kernelStart elf
  return (Region start (kernelEnd elf - start))

-- | The region of memory initially mapped in the VM's virtual space.
-- This region contains the other regions.
-- This region contain an exact number of pages.
getVMRegion :: Stmt Region
getVMRegion = do
  do kernel <- getKernelRegion
     (_, lastAddr) <- getPageTablesInfo
     let start = regionStart kernel
     return (Region start (lastAddr - start + 1))

-- | The region of memory for the start info page.
-- This region is one page (so it contains some padding).
getStartInfoRegion :: Stmt Region
getStartInfoRegion = do
  start <- getStartInfoPageAddr 
  return (Region start pageSize)

-- | Expect zeros after then end of the first region, up to the
-- beginning of the second region.
zeroBetweenRegions :: Region -> Region -> Stmt ()
zeroBetweenRegions before after = zeroBetween (regionEnd before) (regionStart after)


-- | Expect zeros in the given range.  The range includes the first address,
-- but not the last address, so @zeroBetween start end@ would expect 0
-- in the interval [ start, end ).
zeroBetween :: Word64     -- ^ First address expected to be 0.
            -> Word64     -- ^ Address just after end of region.
            -> Stmt ()
zeroBetween before after =
  expectBytes [(before, L.replicate (fromIntegral $ after - before) 0)]

{-
-- | An ELF note. These are stored in sections of SHT_NOTE, segments of type PT_NOTE.
data ElfNote = ElfNote
  { elfNoteType        :: Word64
  , elfNoteName        :: B.ByteString
  , elfNoteDescription :: B.ByteString
  } deriving (Eq,Show)

-- | Interpret the content of a note section or segment.
parseElfNotes :: Elf -> B.ByteString -> [ElfNote]
parseElfNotes elf bytes = runGet loop (L.fromChunks [bytes])
  where
  er   = elfReader (elfData elf)
  ec   = elfClass elf

  loop = do done <- isEmpty
            if done then return []
                    else do note  <- parseElfNote ec er
                            notes <- loop
                            return (note : notes)

parseElfNote :: ElfClass -> ElfReader -> Get ElfNote
parseElfNote elf_class er =
  do namesz   <- fromIntegral `fmap` getWord
     descsz   <- fromIntegral `fmap` getWord
     noteType <- getWord
     name     <- getByteString namesz
     skip (wSize - mod namesz wSize)
     desc     <- getByteString descsz
     let pad = mod (wSize - mod descsz wSize) wSize
     n        <- remaining
     when (fromIntegral pad < n) (skip pad)
     return ElfNote { elfNoteType        = noteType
                    , elfNoteName        = name
                    , elfNoteDescription = desc
                    }

  where (getWord, wSize) = case elf_class of
                             ELFCLASS64 -> (getWord64 er, 8)
                             ELFCLASS32 -> (fromIntegral `fmap` getWord32 er, 4)


-}


