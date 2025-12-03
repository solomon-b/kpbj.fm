{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Get.Templates.Page
  ( template,
  )
where

import {-# SOURCE #-} API (showBlogNewGetLink)
import API.Dashboard.Get.Templates.BlogPost (renderBlogPostTableRow)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTable)
import Data.String.Interpolate (i)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Blog dashboard template
template ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [ShowBlogPosts.Model] ->
  Lucid.Html ()
template _userMeta selectedShow blogPosts = do
  renderHeader selectedShow (length blogPosts)
  renderBlogSection selectedShow blogPosts

-- | Header with stats and action button
renderHeader :: Maybe Shows.Model -> Int -> Lucid.Html ()
renderHeader selectedShow postCount =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 rounded-lg"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "BLOG POSTS"
        Lucid.div_ [Lucid.class_ "text-sm mt-2"] $ do
          Lucid.strong_ [Lucid.class_ "text-gray-400"] "Total Posts: "
          Lucid.span_ [Lucid.class_ "text-white"] $ Lucid.toHtml $ show postCount
      -- Action button
      case selectedShow of
        Just showModel -> do
          let newBlogUrl = Links.linkURI $ showBlogNewGetLink showModel.slug
          Lucid.a_
            [ Lucid.href_ [i|/#{newBlogUrl}|],
              hxGet_ [i|/#{newBlogUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
            ]
            "NEW POST"
        Nothing -> mempty

-- | Blog posts table section
renderBlogSection :: Maybe Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderBlogSection selectedShow blogPosts =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    case blogPosts of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No blog posts yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Share your thoughts with your audience!"
      _ ->
        case selectedShow of
          Nothing -> mempty
          Just showModel ->
            renderTable
              TableConfig
                { headers =
                    [ ColumnHeader "TITLE" AlignLeft,
                      ColumnHeader "PUBLISHED" AlignLeft,
                      ColumnHeader "STATUS" AlignLeft,
                      ColumnHeader "ACTIONS" AlignRight
                    ],
                  wrapperClass = "overflow-x-auto",
                  tableClass = "w-full"
                }
              $ mapM_ (renderBlogPostTableRow showModel) blogPosts
