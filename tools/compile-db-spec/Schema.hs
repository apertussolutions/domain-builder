module Schema where

data VPSchema = VPSchema
  { xenstore_filename    :: String
  , console_filename     :: String
  , additional_filenames :: [String]
  }
  deriving (Show)
