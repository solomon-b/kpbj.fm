-- | The multipart form both break item sections submit.
--
-- PSAs and advertisement spots carry the same fields, so both sections' create
-- and edit routes take this one type. The category is not a field: each section
-- fixes it from its own route.
module API.Dashboard.BreakItems.Form
  ( BreakItemForm (..),
  )
where

--------------------------------------------------------------------------------

import Data.Either (fromRight)
import Data.Text (Text)
import Servant.Multipart (FromMultipart, Mem, fromMultipart, lookupInput)

--------------------------------------------------------------------------------

-- | Form data for a break item upload or edit.
data BreakItemForm = BreakItemForm
  { bifTitle :: Text,
    -- | Staged upload token. Empty on an edit, which never replaces audio.
    bifAudioToken :: Text,
    -- | Duration in seconds, read from the audio file in the browser.
    --
    -- Required on upload, because the break window cannot budget without it.
    bifDurationSeconds :: Text,
    -- | First air date as @YYYY-MM-DD@.
    bifStartsOn :: Text,
    -- | Last air date as @YYYY-MM-DD@. Empty runs open ended.
    bifEndsOn :: Text,
    -- | Priority. Empty reads as zero.
    bifPriority :: Text
  }
  deriving stock (Show)

instance FromMultipart Mem BreakItemForm where
  fromMultipart multipartData =
    BreakItemForm
      <$> lookupInput "title" multipartData
      <*> pure (fromRight "" $ lookupInput "audio_file_token" multipartData)
      <*> pure (fromRight "" $ lookupInput "duration_seconds" multipartData)
      <*> lookupInput "starts_on" multipartData
      <*> pure (fromRight "" $ lookupInput "ends_on" multipartData)
      <*> pure (fromRight "" $ lookupInput "priority" multipartData)
