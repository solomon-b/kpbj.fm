module API.Dashboard.EphemeralUploads.Id.Flag.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata (UserRole (..))
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

-- | "POST /dashboard/ephemeral-uploads/:id/flag"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "id" EphemeralUploads.Id
    :> "flag"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.ReqBody '[Servant.FormUrlEncoded] FlagForm
    :> Servant.Post '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

-- | Form data for flagging an ephemeral upload.
newtype FlagForm = FlagForm
  { ffReason :: Text
  }
  deriving stock (Generic, Show)

instance FormUrlEncoded.FromForm FlagForm where
  fromForm f =
    FlagForm
      <$> FormUrlEncoded.parseUnique "reason" f
