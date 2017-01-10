module DeviceNumber where

import Text.Parsec
import Data.Char (ord)
import Data.Bits

{-
       d0 d0p0  xvda     Xen virtual disk 0 partition 0 (whole disk)
       d1p2     xvdb2    Xen virtual disk 1 partition 2
       d536p37  xvdtq37  Xen virtual disk 536 partition 37
       sdb3              SCSI disk 1 partition 3
       hdc2              IDE disk 2 partition 2
-}

data DiskType = XenDisk | ScsiDisk | IdeDisk
  deriving Show

data DeviceName = DeviceName DiskType Int Int
  deriving Show

computeDeviceName str =
  case parse parseDeviceName "" str of
    Left _ -> Nothing
    Right x -> Just x

parseDeviceName = choice [parseD, parseX, parseH, parseS]

parseD, parseX, parseH, parseS :: Parsec String () DeviceName

parseD = do
  char 'd'
  diskId <- parseNumber
  partId <- option 0 (char 'p' >> parseNumber)
  return (DeviceName XenDisk diskId partId)

parseX = do
  string "xvd"
  diskId <- parseAlpha
  partId <- option 0 parseNumber
  return (DeviceName XenDisk diskId partId)

parseH = do
  string "hd"
  diskId <- parseAlpha
  partId <- option 0 parseNumber
  return (DeviceName IdeDisk diskId partId)

parseS = do
  string "sd"
  diskId <- parseAlpha
  partId <- option 0 parseNumber
  return (DeviceName ScsiDisk diskId partId)

parseNumber = fmap read (many1 digit)
parseAlpha  = fmap readAlpha (many1 (satisfy (\x -> 'a' <= x && x <= 'z')))

readAlpha :: String -> Int
readAlpha = go 0 . map toDigit
  where
  go acc [x]    = 26 * acc + x
  go acc (x:xs) = go (26 * acc + 1 + x) xs

  toDigit a = ord a - ord 'a'

{-
    1 << 28 | disk << 8 | partition      xvd, disks or partitions 16 onwards
   202 << 8 | disk << 4 | partition      xvd, disks and partitions up to 15
     8 << 8 | disk << 4 | partition      sd, disks and partitions up to 15
     3 << 8 | disk << 6 | partition      hd, disks 0..1, partitions 0..63
    22 << 8 | (disk-2) << 6 | partition  hd, disks 2..3, partitions 0..63
    2 << 28 onwards                      reserved for future use
   other values less than 1 << 28        deprecated / reserved
-}

computeDeviceNumber (DeviceName t d p) =
  case t of
    XenDisk  | d >= 16 || p >= 16          -> Just $   1 `shiftL` 28 .|. d     `shiftL` 8 .|. p
             | otherwise                   -> Just $ 202 `shiftL`  8 .|. d     `shiftL` 4 .|. p
    ScsiDisk | d < 16  &&           p < 16 -> Just $   8 `shiftL`  8 .|. d     `shiftL` 4 .|. p
    IdeDisk  | 0 <= d  && d <= 1 && p < 64 -> Just $   3 `shiftL`  8 .|. d     `shiftL` 6 .|. p
             | 2 <= d  && d <= 3 && p < 64 -> Just $  22 `shiftL`  8 .|. (d-2) `shiftL` 6 .|. p
    _ -> Nothing
