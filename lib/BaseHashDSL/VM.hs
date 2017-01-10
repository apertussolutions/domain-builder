module VM (VM(..), Expect(..), newVM, insertStrict, insertOverlap, compressVM) where

import qualified Data.ByteString.Lazy as S
import Data.Word (Word64)

pageSize :: Num a => a
pageSize = 4096


data Expect = ExpectBytes     S.ByteString
            | ExpectUnmapped  Word64          -- How many unmapped pages

sizeOfExpect :: Expect -> Word64
sizeOfExpect (ExpectBytes bs)   = fromIntegral (S.length bs)
sizeOfExpect (ExpectUnmapped n) = n * pageSize


newtype VM  = VM [(Word64, Expect)]   -- Sorted, non-overlapping

newVM :: VM
newVM = VM []


type Err = String

insertStrict :: Word64 -> Expect -> VM -> (VM, [Err])
insertStrict ourStart (ExpectUnmapped _) vm
  | ourStart `mod` pageSize /= 0  =
      (vm, ["ExpectUnmapped address not aligned: " ++ show ourStart])

insertStrict ourStart expect (VM vm0) = let (a,b) = ins vm0 in (VM a, b)
  where
  ourEnd = ourStart + sizeOfExpect expect

  ins []              = ([(ourStart,expect)], [])
  ins vm@((start,val) : more)
    | ourStart >= end = let (vm1, errs) = ins more in ((start,val) : vm1, errs)
    | ourEnd <= start = ((ourStart,expect) : vm, [])
    | otherwise       = (vm, ["Overlap"])

    where end = start + sizeOfExpect val


insertOverlap :: Word64 -> S.ByteString -> VM -> VM
insertOverlap origStart bs0 (VM vm0) = VM (ins origStart bs0 vm0)
  where
  ins ourStart bs []  = [(ourStart, ExpectBytes bs)]
  ins ourStart bs vm@((start,val) : more)
    | S.null bs       = vm
    | ourStart >= end = (start, val) : ins ourStart bs more
    | otherwise       = let nowLen      = fromIntegral (start - ourStart)
                            (now,later) = S.splitAt nowLen bs
                        in (ourStart, ExpectBytes now) : (start,val) :
                              ins end (S.drop (fromIntegral size) later) more

    where end  = start + size
          size = sizeOfExpect val


compressVM :: VM -> VM
compressVM (VM vm) = VM (foldr addRegion [] vm)
  where
  addRegion (_, ExpectBytes bs) xs
    | S.null bs = xs
  addRegion (a1, ExpectBytes b1) ((a2, ExpectBytes b2) : xs)
    | a1 + fromIntegral (S.length b1) == a2 = (a1, ExpectBytes (S.append b1 b2)) : xs
  addRegion x xs = x : xs
