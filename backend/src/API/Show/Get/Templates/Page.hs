{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Get.Templates.Page
  ( template,
    notFoundTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showsGetLink)
import API.Show.Get.Templates.Episode (renderEpisodeCard, renderLatestEpisode)
import API.Show.Get.Templates.ShowHeader (renderBreadcrumb, renderShowHeader)
import API.Show.Get.Templates.Sidebar (renderHostBio, renderRecentBlogPosts, renderShowStats)
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.HostDetails qualified as HostDetails
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

-- | Template for show not found
notFoundTemplate :: Text -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Show Not Found"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ "The show '" <> Lucid.toHtml slug <> "' could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BROWSE ALL SHOWS"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BROWSE ALL SHOWS"

-- | Main show page template
template :: Shows.Model -> [Episodes.Model] -> Maybe [EpisodeTrack.Model] -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.Model] -> Maybe HostDetails.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts = do
  -- Breadcrumb
  renderBreadcrumb showModel

  -- Show Header
  renderShowHeader showModel episodes hosts schedules

  -- Content Tabs Navigation
  Lucid.div_ [Lucid.class_ "mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800"] $ do
      Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
        Lucid.button_ [Lucid.class_ "py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5"] "Episodes"
        Lucid.button_ [Lucid.class_ "py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800"] "Blog"

  -- Main Content Grid
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 w-full"] $ do
    -- Episodes Section
    Lucid.section_ [Lucid.class_ "lg:col-span-2"] $ do
      if null episodes
        then do
          Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
            Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Episodes Yet"
            Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] "This show hasn't published any episodes yet. Check back soon!"
        else do
          -- Featured/Latest Episode with tracks
          case (episodes, latestEpisodeTracks) of
            (latestEpisode : otherEpisodes, Just tracks) -> do
              renderLatestEpisode latestEpisode tracks

              -- Other Episodes
              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ renderEpisodeCard otherEpisodes
            (latestEpisode : otherEpisodes, Nothing) -> do
              -- Fallback if tracks failed to load
              renderLatestEpisode latestEpisode []

              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ renderEpisodeCard otherEpisodes
            _ -> mempty

    -- Sidebar
    Lucid.aside_ [Lucid.class_ "flex flex-col gap-8"] $ do
      -- Host Bio (show primary host)
      case hosts of
        (primaryHost : _) -> renderHostBio primaryHost mHostDetails
        [] -> mempty

      -- Show Stats
      renderShowStats showModel episodes schedules

      -- Recent Blog Posts
      renderRecentBlogPosts blogPosts
