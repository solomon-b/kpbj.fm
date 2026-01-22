module API.Dashboard.EphemeralUploads.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FromMultipart, Mem, MultipartForm, fromMultipart, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/ephemeral-uploads/new"
    ( "dashboard"
        :> "ephemeral-uploads"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem FormData
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Form data for ephemeral upload
data FormData = FormData
  { fdTitle :: Text,
    fdAudioToken :: Text
  }
  deriving stock (Show)

instance FromMultipart Mem FormData where
  fromMultipart multipartData =
    FormData
      <$> lookupInput "title" multipartData
      <*> lookupInput "audio_file_token" multipartData
