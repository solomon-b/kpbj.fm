module API.Uploads.Audio.Post.Route where

--------------------------------------------------------------------------------

import API.Uploads.Types (AudioUploadForm, UploadResponse)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Origin (Origin)
import GHC.Generics (Generic)
import Servant ((:>))
import Servant qualified
import Servant.Multipart (Mem, MultipartForm)

--------------------------------------------------------------------------------

-- | CORS headers for cross-origin upload requests.
-- This allows uploads from the main domain to the uploads subdomain.
type CorsHeaders =
  '[ Servant.Header "Access-Control-Allow-Origin" Text,
     Servant.Header "Access-Control-Allow-Credentials" Text
   ]

-- | "POST /api/uploads/audio"
type Route =
    "api"
      :> "uploads"
      :> "audio"
      :> Servant.Header "Cookie" Cookie
      :> Servant.Header "Origin" Origin
      :> MultipartForm Mem AudioUploadForm
      :> Servant.Post '[Servant.JSON] (Servant.Headers CorsHeaders UploadApiResponse)

-- | OPTIONS preflight route for CORS.
type OptionsRoute =
  "api"
    :> "uploads"
    :> "audio"
    :> Servant.Header "Origin" Origin
    :> Servant.Verb
         'Servant.OPTIONS
         204
         '[Servant.JSON]
         ( Servant.Headers
             '[ Servant.Header "Access-Control-Allow-Origin" Text,
                Servant.Header "Access-Control-Allow-Methods" Text,
                Servant.Header "Access-Control-Allow-Headers" Text,
                Servant.Header "Access-Control-Allow-Credentials" Text,
                Servant.Header "Access-Control-Max-Age" Text
              ]
             Servant.NoContent
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
