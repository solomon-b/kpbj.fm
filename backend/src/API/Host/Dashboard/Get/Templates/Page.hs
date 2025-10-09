{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Page
  ( template,
  )
where

import {-# SOURCE #-} API (episodeUploadGetLink)
import API.Host.Dashboard.Get.Templates.BlogPost (renderBlogPostCard)
import API.Host.Dashboard.Get.Templates.Episode (renderEpisodeCard)
import API.Host.Dashboard.Get.Templates.Schedule (renderScheduleSection)
import API.Host.Dashboard.Get.Templates.Stats (renderStatsSection)
import Data.String.Interpolate (i)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Host Dashboard template
template :: UserMetadata.Model -> [Shows.Model] -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
template userMeta userShows recentEpisodes blogPosts = do
  -- Dashboard Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "HOST DASHBOARD"
        case userShows of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] "No shows assigned"
          (primaryShow : _) -> do
            Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
              Lucid.strong_ "Show: "
              Lucid.toHtml primaryShow.title
              " • "
              Lucid.strong_ "Host: "
              Lucid.toHtml userMeta.mDisplayName
            Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
              Lucid.strong_ "Schedule: "
              Lucid.span_ "TBD" -- TODO: Add schedule info
              " • "
              Lucid.strong_ "Genre: "
              maybe "TBD" Lucid.toHtml primaryShow.genre
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        case userShows of
          [] -> Lucid.span_ [Lucid.class_ "text-gray-400"] "No show assigned"
          (_primaryShow : _) -> do
            Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"
            -- TODO: Add link to show profile page when ready
            Lucid.span_ [Lucid.class_ "text-blue-300 text-sm"] "VIEW PUBLIC PAGE"

  -- Quick Actions
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "QUICK ACTIONS"
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-3 gap-4"] $ do
      Lucid.a_ [Lucid.href_ [i|/#{episodeUploadGetUrl}|], hxGet_ [i|/#{episodeUploadGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "bg-blue-600 text-white p-4 font-bold hover:bg-blue-700 transition-colors block text-center"] $ do
        "🎵 PREPARE SHOW"
      Lucid.a_ [Lucid.href_ "#new-blog-post", Lucid.class_ "bg-green-600 text-white p-4 font-bold hover:bg-green-700 transition-colors block text-center"] $ do
        "📝 NEW BLOG POST"
      Lucid.a_ [Lucid.href_ "#edit-profile", Lucid.class_ "bg-purple-600 text-white p-4 font-bold hover:bg-purple-700 transition-colors block text-center"] $ do
        "✏️ EDIT PROFILE"

  -- Main Dashboard Grid
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 mb-8 w-full"] $ do
    -- Recent Episodes Section
    Lucid.div_ [Lucid.class_ "lg:col-span-2"] $ do
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "RECENT EPISODES"

        case recentEpisodes of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
            Lucid.p_ "No episodes uploaded yet."
            Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Use 'PREPARE SHOW' to upload your first episode."
          _ -> Lucid.div_ [Lucid.class_ "space-y-4"] $ do
            mapM_ renderEpisodeCard $ take 3 recentEpisodes

      -- Recent Blog Posts
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-4"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold"] "RECENT BLOG POSTS"
          Lucid.button_ [Lucid.class_ "bg-green-600 text-white px-4 py-2 text-sm font-bold hover:bg-green-700"] $ do
            "NEW POST"

        case blogPosts of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
            Lucid.p_ "No blog posts published yet."
            Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Share your thoughts with your audience!"
          _ -> Lucid.div_ [Lucid.class_ "space-y-4"] $ do
            mapM_ renderBlogPostCard $ take 3 blogPosts

    -- Sidebar
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      -- Show Stats
      renderStatsSection userShows recentEpisodes blogPosts

      -- Next Show Schedule
      renderScheduleSection userShows
  where
    episodeUploadGetUrl :: Links.URI
    episodeUploadGetUrl = Links.linkURI episodeUploadGetLink
