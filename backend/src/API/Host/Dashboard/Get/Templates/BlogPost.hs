{-# LANGUAGE OverloadedRecordDot #-}

module API.Host.Dashboard.Get.Templates.BlogPost
  ( renderBlogPostCard,
  )
where

import Data.Text qualified as Text
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlog
import Lucid qualified

-- | Render individual blog post card
renderBlogPostCard :: ShowBlog.ShowBlogPostModel -> Lucid.Html ()
renderBlogPostCard post = do
  Lucid.div_ [Lucid.class_ "border border-gray-300 p-4"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start mb-2"] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ "font-bold"] $ Lucid.toHtml post.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
          Lucid.toHtml (show post.createdAt)
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.button_ [Lucid.class_ "bg-gray-600 text-white px-3 py-1 text-xs font-bold hover:bg-gray-700"] "EDIT"
        Lucid.button_ [Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"] "DELETE"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-700 mb-2"] $ do
      let content = post.content
      Lucid.toHtml $ Text.take 120 content
      if Text.length content > 120 then "..." else ""

    Lucid.div_ [Lucid.class_ "flex justify-between items-center text-xs text-gray-500"] $ do
      Lucid.div_ $ do
        "Status: "
        Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "Published"
      Lucid.div_ "💬 - comments • 👀 - views" -- TODO: Add real stats
