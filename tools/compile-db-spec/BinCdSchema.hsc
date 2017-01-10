{-# LANGUAGE ForeignFunctionInterface #-}
module BinCdSchema (saveCdSchema) where

#include <control-domain-schema/encode.h>
import Foreign
import Foreign.C
import Foreign.C.String
import System.IO
import Control.Monad(when,zipWithM_)
import SimpleKeyValue(Value(..),Atom(..))

maxSize :: Int
maxSize = 4096

foreign import ccall unsafe
  cd_schema_encode :: Ptr a -> Ptr b -> CSize -> IO CInt

saveCdSchema :: FilePath -> [(String,Value)] -> IO ()
saveCdSchema file entries = allocaBytes maxSize $ \buf ->
  do p <- mallocCdSchema entries
     size <- cd_schema_encode p buf (fromIntegral maxSize)
     when (size < 0) $
       fail $ "Failed to save control domain schema (" ++ show size ++ ")"
     h <- openBinaryFile file WriteMode
     hPutBuf h buf (fromIntegral size)
     hClose h


mallocCdSchema :: [(String,Value)] -> IO (Ptr ())
mallocCdSchema entries =
  do p <- mallocBytes (#size control_domain_schema)
     let len = length entries
     (#poke control_domain_schema, entry_num) p (fromIntegral len :: CUInt)

     p1 <- mallocBytes (len * (#size control_domain_schema_entry))
     let ptrs = iterate (`plusPtr` (#size control_domain_schema_entry)) p1
     zipWithM_ pokeEntry ptrs entries
     (#poke control_domain_schema, entries) p p1
     return p

pokeEntry :: Ptr () -> (String,Value) -> IO ()
pokeEntry p (k,Atom (String v)) =
  do (#poke control_domain_schema_entry, key)   p =<< newCString k
     (#poke control_domain_schema_entry, value) p =<< newCString v
pokeEntry _ x = fail $ "Unexpected non-string value: " ++ show x




