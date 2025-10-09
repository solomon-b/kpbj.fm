module API.Host.Dashboard.Get.Templates.Stats
  ( renderStatsSection,
  )
where

import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

-- | Render stats sidebar section
renderStatsSection :: [Shows.Model] -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderStatsSection userShows recentEpisodes blogPosts = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "SHOW STATISTICS"
    Lucid.div_ [Lucid.class_ "space-y-3 text-sm"] $ do
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Total Episodes:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length recentEpisodes))
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Total Downloads:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] "-" -- TODO: Add real download stats
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Blog Posts:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length blogPosts))
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Shows:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length userShows))
