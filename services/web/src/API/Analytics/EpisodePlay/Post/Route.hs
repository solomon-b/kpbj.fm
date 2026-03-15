-- | Route definition for POST /api/analytics/episode-play.
module API.Analytics.EpisodePlay.Post.Route
  ( Route,
    EpisodePlayRequest (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | Request body for POST /api/analytics/episode-play.
newtype EpisodePlayRequest = EpisodePlayRequest
  { episodeId :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- | @POST /api/analytics/episode-play@
--
-- Records that a user played an archived episode via the browser audio player.
-- No authentication required (public endpoint).
type Route =
  "api"
    :> "analytics"
    :> "episode-play"
    :> Servant.ReqBody '[Servant.JSON] EpisodePlayRequest
    :> Servant.PostNoContent
