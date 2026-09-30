-- | The multipart form both break item sections submit.
--
-- PSAs and underwriting announcements share one form type. The underwriting
-- section adds the underwriter and spots per month, which a PSA leaves empty.
-- The category is not a field. Each section fixes it from its own route.
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
    -- | Duration in seconds, measured by @ffprobe@ when the audio was staged
    -- and returned in the upload response.
    --
    -- Present on upload, because the break window cannot budget without it.
    -- Empty on an edit, which never replaces audio.
    bifDurationSeconds :: Text,
    -- | First air date as @YYYY-MM-DD@.
    bifStartsOn :: Text,
    -- | Last air date as @YYYY-MM-DD@. Empty runs open ended.
    bifEndsOn :: Text,
    -- | Underwriter id. Empty for a PSA.
    bifUnderwriterId :: Text,
    -- | Spots per month. Empty for a PSA.
    bifSpotsPerMonth :: Text
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
      <*> pure (fromRight "" $ lookupInput "underwriter_id" multipartData)
      <*> pure (fromRight "" $ lookupInput "spots_per_month" multipartData)
