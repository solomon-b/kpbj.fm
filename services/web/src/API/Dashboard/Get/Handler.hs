{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Data.String.Interpolate (i)
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

--------------------------------------------------------------------------------

-- | Redirect /dashboard to /dashboard/episodes
handler ::
  AppM (Lucid.Html ())
handler =
  pure $ redirectTemplate [i|/#{dashboardEpisodesRedirectUrl}|]
