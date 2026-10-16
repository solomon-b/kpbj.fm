-- | The storage prefix of staged uploads.
--
-- Every staged upload goes to @temp/staging/@. A claim moves the file to the
-- archive under @audio/@ or @images/@. Code that deletes staged files checks
-- the path with 'isStagingPath' first, so it cannot delete an archive file.
module Effects.Storage.Staging
  ( stagingPrefix,
    isStagingPath,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType (..), buildStagingKey)

--------------------------------------------------------------------------------

-- | The key prefix of every staged upload: @temp/staging/@.
stagingPrefix :: Text
stagingPrefix = buildStagingKey TempBucket "staging" ""

-- | True only for a file name inside 'stagingPrefix'.
--
-- The path must start with the prefix, have a file name after it, and have
-- no @..@ segment.
isStagingPath :: Text -> Bool
isStagingPath path =
  case Text.stripPrefix stagingPrefix path of
    Nothing -> False
    Just rest -> not (Text.null rest) && ".." `notElem` Text.splitOn "/" path
