{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Get.Templates.Sidebar
  ( renderHostBio,
    renderShowStats,
    renderRecentBlogPosts,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (unless, when)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.ShowBlog qualified as ShowBlog
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render Host Bio section
renderHostBio :: Show.ShowHostWithUser -> Maybe Show.HostDetailsModel -> Lucid.Html ()
renderHostBio host mHostDetails = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] $
      "About " <> Lucid.toHtml (host.displayName)

    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      -- Host avatar
      Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border-2 border-gray-600 rounded-full mx-auto mb-3 flex items-center justify-center overflow-hidden"] $ do
        case host.avatarUrl of
          Just avatarUrl -> Lucid.img_ [Lucid.src_ avatarUrl, Lucid.alt_ "Host avatar", Lucid.class_ "w-full h-full object-cover"]
          Nothing -> Lucid.div_ [Lucid.class_ "text-xs"] "[HOST]"

      -- Host stats
      Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
        Lucid.div_ [Lucid.class_ "font-bold"] $ do
          "Host since: "
          let year = Text.pack $ formatTime defaultTimeLocale "%Y" (host.joinedAt)
          Lucid.toHtml year

    -- Bio text
    case mHostDetails >>= (.bio) of
      Just bioText -> Lucid.p_ [Lucid.class_ "text-sm mb-4 leading-relaxed"] $ Lucid.toHtml bioText
      Nothing -> mempty

    -- Contact and links
    Lucid.div_ [Lucid.class_ "text-xs space-y-1"] $ do
      case host.fullName of
        fullName | fullName /= "" ->
          Lucid.div_ $ do
            Lucid.strong_ "Name: "
            Lucid.toHtml fullName
        _ -> mempty

      case mHostDetails >>= (.websiteUrl) of
        Just website ->
          Lucid.div_ $ do
            Lucid.strong_ "Website: "
            Lucid.a_ [Lucid.href_ website, Lucid.target_ "_blank", Lucid.class_ "underline hover:no-underline"] $ Lucid.toHtml website
        Nothing -> mempty

      case mHostDetails >>= (.instagramHandle) of
        Just instagram ->
          Lucid.div_ $ do
            Lucid.strong_ "Instagram: "
            Lucid.toHtml $ "@" <> instagram
        Nothing -> mempty

      case mHostDetails >>= (.twitterHandle) of
        Just twitter ->
          Lucid.div_ $ do
            Lucid.strong_ "Twitter: "
            Lucid.toHtml $ "@" <> twitter
        Nothing -> mempty

-- | Render Show Stats section
renderShowStats :: Show.ShowModel -> [Episode.EpisodeModel] -> [Show.ShowScheduleModel] -> Lucid.Html ()
renderShowStats showModel episodes schedules = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Show Stats"

    Lucid.div_ [Lucid.class_ "space-y-3 text-sm"] $ do
      -- Total Episodes
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ "Total Episodes:"
        Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml $ Prelude.show $ length episodes

      -- Average Duration
      unless (null episodes) $ do
        let durations = [d | ep <- episodes, Just d <- [ep.durationSeconds]]
            avgDuration = if null durations then 0 else sum durations `div` fromIntegral (length durations)
            avgMinutes = avgDuration `div` 60
        when (avgDuration > 0) $ do
          Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
            Lucid.span_ "Average Duration:"
            Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml (show avgMinutes) <> "min"

      -- Show Status
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ "Status:"
        Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ showModel.status

      -- Next Show (if there's a schedule)
      unless (null schedules) $ do
        case schedules of
          (schedule : _) -> do
            Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
              Lucid.span_ "Next Show:"
              let dayName = case schedule.dayOfWeek of
                    0 -> "Sun"
                    1 -> "Mon"
                    2 -> "Tue"
                    3 -> "Wed"
                    4 -> "Thu"
                    5 -> "Fri"
                    6 -> "Sat"
                    _ -> "???"
                  startTime = schedule.startTime
              Lucid.span_ [Lucid.class_ "font-bold text-green-700"] $ Lucid.toHtml $ dayName <> " " <> startTime
          _ -> mempty

-- | Render Recent Blog Posts section
renderRecentBlogPosts :: [ShowBlog.ShowBlogPostModel] -> Lucid.Html ()
renderRecentBlogPosts blogPosts = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Recent from Host Blog"

    if null blogPosts
      then Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] "No blog posts yet."
      else do
        Lucid.div_ [Lucid.class_ "space-y-4"] $ do
          mapM_ renderBlogPostPreview (take 3 blogPosts)
  where
    renderBlogPostPreview :: ShowBlog.ShowBlogPostModel -> Lucid.Html ()
    renderBlogPostPreview post = do
      Lucid.article_ [Lucid.class_ "border-l-4 border-gray-800 pl-4"] $ do
        case post.publishedAt of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mb-1"] $ Lucid.toHtml dateStr
          Nothing -> mempty

        Lucid.h4_ [Lucid.class_ "font-bold text-sm mb-2"] $ Lucid.toHtml (post.title)

        case post.excerpt of
          Just excerpt -> do
            let truncated = if Text.length excerpt > 150 then Text.take 150 excerpt <> "..." else excerpt
            Lucid.p_ [Lucid.class_ "text-xs text-gray-700 leading-relaxed mb-2"] $ Lucid.toHtml truncated
          Nothing -> mempty

        Lucid.a_ [Lucid.href_ "#", Lucid.class_ "text-xs text-gray-800 underline hover:no-underline"] "Read more"
