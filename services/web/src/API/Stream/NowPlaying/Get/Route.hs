-- | Route definition for GET /api/stream/now-playing.
module API.Stream.NowPlaying.Get.Route
  ( Route,
    NowPlaying (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/stream/now-playing"
--
-- Returns the title of what airs now, and the page of the episode when an
-- episode airs. Used by the top player.
type Route =
  "api"
    :> "stream"
    :> "now-playing"
    :> Servant.Get '[Servant.JSON] NowPlaying

-- | What the stream plays now.
--
-- Serializes to @{"title": "...", "url": "..."}@, with a null @"url"@ when no
-- episode plays.
data NowPlaying = NowPlaying
  { npTitle :: Text,
    -- | The page of the episode on air. Nothing when no episode plays.
    npUrl :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON NowPlaying where
  toJSON nowPlaying =
    object ["title" .= nowPlaying.npTitle, "url" .= nowPlaying.npUrl]
