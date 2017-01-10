{-# LANGUAGE MultiParamTypeClasses #-}
module Spec where

import Control.Monad (replicateM_)
import Data.Word
import Data.Maybe(fromMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified MonadLib
import Numeric(readHex)

data Spec = Spec
  { domid             :: Maybe Word64
  , max_vcpus         :: Word64
  , allocate_kbs      :: Word64
  , max_kbs           :: Word64
  , dom_flags         :: Word64

  , kernel_name       :: FilePath
  , ramdisk_name      :: Maybe FilePath
  , command_line      :: String
  , kernel_hash       :: Maybe FilePath
  , ramdisk_hash      :: Maybe FilePath
  , security_context  :: Maybe String

  , vmSpec            :: VMSpec
  }
  deriving (Show)

data VMSpec = HVM HVMSpec | PV PVSpec
  deriving (Show)

data HVMSpec = HVMSpec
  {
  }
  deriving (Show)

data PVSpec = PVSpec
  { irq_bitmap        :: [Bool]
  , iomems            :: [Range]
  , ioports           :: [Range]
  }
  deriving (Show)

data Range = Range
  { start             :: Word64
  , how_many          :: Word64
  , enabled           :: Bool
  }
  deriving (Show)


--------------------------------------------------------------------------------

bitmapToBytes :: [Bool] -> [Word8]
bitmapToBytes []  = []
bitmapToBytes xs  = let (as,bs) = splitAt 8 xs
                    in toByte as : bitmapToBytes bs
  where toByte xs   = sum (zipWith pick xs pows)
        pows        = 1 : [ 2 * p | p <- pows ]
        pick yes p  = if yes then p else 0
