module API.Dashboard.Episodes.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/episodes/:show_slug/:episode_number"
--
-- Soft delete (archive) route for episodes.
-- This is restricted to staff or higher roles. It sets the episode status
-- to 'deleted' but preserves the record in the database. Use this to remove
-- published episodes from public view while maintaining history.
--
-- For discarding draft episodes, hosts should use the DiscardDraft endpoint instead.
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
