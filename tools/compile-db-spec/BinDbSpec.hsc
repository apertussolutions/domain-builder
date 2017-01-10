{-# LANGUAGE ForeignFunctionInterface #-}
module BinDbSpec (saveSpec) where

import Spec
import Foreign
import Foreign.C
import Data.Maybe
import System.IO

#include "db-api/encode.h"

saveSpec :: FilePath -> Spec -> IO ()
saveSpec file spec =
  allocaBytes maxSize $ \outBuf ->
    do struct <- mk_db_boot_spec spec
       sz <- db_boot_spec_encode struct outBuf maxSize
       h <- openBinaryFile file WriteMode
       hPutBuf h outBuf (fromIntegral sz)
       hClose h

  where maxSize :: Num a => a
        maxSize = 4096


foreign import ccall unsafe
  db_boot_spec_encode :: Ptr a -> Ptr b -> CSize -> IO CInt


--------------------------------------------------------------------------------
-- The functions bellow convert a 'Spec' value into a C-struct of
-- the corresponding format, which can be serialized using the C-encode
-- function.
-- NOTE: The C-struct is malloced and, currently, we don't free it.

mk_db_boot_spec :: Spec -> IO (Ptr a)
mk_db_boot_spec s =
  do p <- mallocBytes (#size db_boot_spec)
     mk_db_spec s ((#ptr db_boot_spec,spec) p)

     (#poke db_boot_spec,kernel_name)       p =<< newCString         (kernel_name s)
     (#poke db_boot_spec,ramdisk_name)      p =<< newNullableCString (ramdisk_name s)
     (#poke db_boot_spec,kernel_hash_name)  p =<< newNullableCString (kernel_hash s)
     (#poke db_boot_spec,ramdisk_hash_name) p =<< newNullableCString (ramdisk_hash s)
     (#poke db_boot_spec,security_context)  p =<< newNullableCString (security_context s)

     return p

newNullableCString :: Maybe String -> IO CString
newNullableCString Nothing    = return nullPtr
newNullableCString (Just str) = newCString str

mk_db_spec :: Spec -> Ptr a -> IO ()
mk_db_spec s p =
  do (#poke db_spec,domid)            p $ fromMaybe 0 $ domid s
     (#poke db_spec,max_vcpus)        p $ max_vcpus s
     (#poke db_spec,allocate_kbs)     p $ allocate_kbs s
     (#poke db_spec,max_kbs)          p $ max_kbs s
     (#poke db_spec,dom_flags)        p $ dom_flags s
     (#poke db_spec,command_line)     p =<< newCString (command_line s)
     mk_vm (vmSpec s) ((#ptr db_spec,vm) p)

mk_vm :: VMSpec -> Ptr a -> IO ()
mk_vm (HVM HVMSpec) p = poke (castPtr p :: Ptr CInt) (#const hvm)
mk_vm (PV perms)    p = do poke (castPtr p :: Ptr CInt) (#const pv)
                           mk_perms perms (plusPtr p (#size int))

mk_perms :: PVSpec -> Ptr a -> IO ()
mk_perms s p =
  do let memsz  = sz (iomems s)
         portsz = sz (ioports s)
         bmsz   = (sz (irq_bitmap s) + 7) `div` 8

     memptr  <- mallocBytes (fromIntegral memsz  * (#size db_spec_range))
     portptr <- mallocBytes (fromIntegral portsz * (#size db_spec_range))
     bmptr   <- mallocBytes (fromIntegral bmsz)

     mk_ranges (iomems s)  memptr
     mk_ranges (ioports s) portptr
     pokeArray bmptr (bitmapToBytes (irq_bitmap s))

     (#poke db_perms_t,iomem_count)     p memsz
     (#poke db_perms_t,iomem)           p memptr
     (#poke db_perms_t,ioport_count)    p portsz
     (#poke db_perms_t,ioport)          p portptr
     (#poke db_perms_t,irq_bitmap_size) p bmsz
     (#poke db_perms_t,irq_bitmap)      p bmptr

  where sz :: [a] -> CUInt
        sz = fromIntegral . length

mk_ranges :: [Range] -> Ptr a -> IO ()
mk_ranges [] _       = return ()
mk_ranges (r : rs) p = do mk_range r p
                          mk_ranges rs (plusPtr p (#size db_spec_range))

mk_range :: Range -> Ptr a -> IO ()
mk_range r p =
  do (#poke db_spec_range,start)    p $ start r
     (#poke db_spec_range,how_many) p $ how_many r
     (#poke db_spec_range,enabled)  p $ if enabled r then 1 :: Word64 else 0


