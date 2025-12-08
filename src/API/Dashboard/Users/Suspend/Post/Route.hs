module API.Dashboard.Users.Suspend.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/users/:id/suspend"
    ( "dashboard"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "suspend"
        :> Servant.Header "Cookie" Cookie
        :> Servant.ReqBody '[Servant.FormUrlEncoded] SuspendForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for suspending a user
newtype SuspendForm = SuspendForm
  { sfReason :: Text
  }
  deriving stock (Generic, Show)

instance FormUrlEncoded.FromForm SuspendForm where
  fromForm f =
    SuspendForm
      <$> FormUrlEncoded.parseUnique "reason" f
