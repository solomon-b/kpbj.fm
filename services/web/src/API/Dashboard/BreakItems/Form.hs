-- | The multipart form both break item sections submit.
--
-- PSAs and underwriting announcements share one form type. The underwriting
-- section adds the underwriter, which a PSA leaves empty.
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
    -- The staged upload row holds the audio length.
    bifAudioToken :: Text,
    -- | First air date as @YYYY-MM-DD@.
    bifStartsOn :: Text,
    -- | Last air date as @YYYY-MM-DD@. Empty runs open ended.
    bifEndsOn :: Text,
    -- | Underwriter id. Empty for a PSA.
    bifUnderwriterId :: Text
  }
  deriving stock (Show)

instance FromMultipart Mem BreakItemForm where
  fromMultipart multipartData =
    BreakItemForm
      <$> lookupInput "title" multipartData
      <*> pure (fromRight "" $ lookupInput "audio_file_token" multipartData)
      <*> lookupInput "starts_on" multipartData
      <*> pure (fromRight "" $ lookupInput "ends_on" multipartData)
      <*> pure (fromRight "" $ lookupInput "underwriter_id" multipartData)
