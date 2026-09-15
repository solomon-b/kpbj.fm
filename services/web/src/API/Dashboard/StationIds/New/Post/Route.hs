module API.Dashboard.StationIds.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FromMultipart, Mem, MultipartForm, fromMultipart, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/station-ids/new"
type Route =
  "dashboard"
    :> "station-ids"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem FormData
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)

--------------------------------------------------------------------------------

-- | Form data for station ID upload
data FormData = FormData
  { fdTitle :: Text,
    fdAudioToken :: Text,
    -- | Duration in seconds, read from the audio file in the browser.
    --
    -- The break window subtracts the station ID's length from its budget before
    -- it picks break items. Absent when the browser could not read the file.
    fdDurationSeconds :: Maybe Text
  }
  deriving stock (Show)

instance FromMultipart Mem FormData where
  fromMultipart multipartData =
    FormData
      <$> lookupInput "title" multipartData
      <*> lookupInput "audio_file_token" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "duration_seconds" multipartData))
