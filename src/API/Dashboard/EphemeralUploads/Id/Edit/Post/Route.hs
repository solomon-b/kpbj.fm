module API.Dashboard.EphemeralUploads.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Either (fromRight)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FromMultipart, Mem, MultipartForm, fromMultipart, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/ephemeral-uploads/:ephemeral_upload_id/edit"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "ephemeral_upload_id" EphemeralUploads.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem FormData
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

-- | Form data for ephemeral upload edit
data FormData = FormData
  { fdTitle :: Text,
    -- | Optional new audio file token (empty if not re-uploading)
    fdAudioToken :: Text
  }
  deriving stock (Show)

instance FromMultipart Mem FormData where
  fromMultipart multipartData =
    FormData
      <$> lookupInput "title" multipartData
      <*> pure (fromRight "" $ lookupInput "audio_file_token" multipartData)
