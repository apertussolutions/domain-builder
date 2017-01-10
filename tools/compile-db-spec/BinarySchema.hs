module BinarySchema where

import Data.Serialize.Put
import Data.Word

import Schema

maximumFilepath = 39 :: Int

schemaVersion = 1 :: Word32

paddedString str = take (maximumFilepath + 1) (str ++ repeat '\0')
putFixedString str = mapM_ (putWord8 . toEnum . fromEnum) (paddedString str)

putVPSchema schema = runPut $ do
  putWord32host schemaVersion
  putFixedString (console_filename schema)
  putFixedString (xenstore_filename schema)
  putWord32host  (fromIntegral (length (additional_filenames schema)))
  mapM_ putFixedString (additional_filenames schema)
  putFixedString ""
