module API.Dashboard.Users.ResetPassword.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/users/:id/reset-password"
--
-- Admin-only: generates a new random password for the target user, hashes and
-- stores it, and clears all of the user's sessions. Returns a modal fragment
-- showing the plaintext password once.
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> "reset-password"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Post '[HTML] (Lucid.Html ())
