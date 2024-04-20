-- | General purpose utility functions
module Utils where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL

--------------------------------------------------------------------------------

-- | Read and parse JSON data from disk.
readJSON :: (FromJSON a) => FilePath -> IO a
readJSON path' = do
  raw <- Aeson.eitherDecode <$> BL.readFile path'
  case raw of
    Left err -> error $ show err
    Right json -> pure json
