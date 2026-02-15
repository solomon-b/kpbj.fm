{-# LANGUAGE DeriveAnyClass #-}

module API.Dashboard.StreamSettings.ForceEpisode.Post.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Domain.Types.Cookie (Cookie)
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/force-episode"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "force-episode"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] ForceEpisodeForm
    :> Servant.Post '[HTML] (Lucid.Html ())

-- | Form data for force-playing an episode.
newtype ForceEpisodeForm = ForceEpisodeForm
  { episodeId :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromForm)
