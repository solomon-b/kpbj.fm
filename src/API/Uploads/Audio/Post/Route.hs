module API.Uploads.Audio.Post.Route where

--------------------------------------------------------------------------------

import API.Uploads.Types (AudioUploadForm, UploadResponse)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Origin (Origin)
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Servant ((:>))
import Servant qualified
import Servant.Multipart (Mem, MultipartForm)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /api/uploads/audio"
    ( "api"
        :> "uploads"
        :> "audio"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "Origin" Origin
        :> MultipartForm Mem AudioUploadForm
        :> Servant.Post '[Servant.JSON] UploadApiResponse
    )

--------------------------------------------------------------------------------
-- Response wrapper that handles both success and error cases

-- | Wrapper for upload API responses.
--
-- Returns JSON with either the upload result or an error message.
data UploadApiResponse
  = UploadSuccess UploadResponse
  | UploadError Text
  deriving stock (Generic, Show)

instance ToJSON UploadApiResponse where
  toJSON (UploadSuccess resp) =
    Aeson.object
      [ "success" Aeson..= True,
        "data" Aeson..= resp
      ]
  toJSON (UploadError msg) =
    Aeson.object
      [ "success" Aeson..= False,
        "error" Aeson..= msg
      ]
