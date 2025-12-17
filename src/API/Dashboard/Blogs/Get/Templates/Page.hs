module API.Dashboard.Blogs.Get.Templates.Page
  ( template,
  )
where

import API.Dashboard.Get.Templates.BlogPost (renderBlogPostTableRow)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTable)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Responsive (cls)

-- | Blog dashboard template (stats are now in the top bar)
template ::
  Maybe Shows.Model ->
  [ShowBlogPosts.Model] ->
  Lucid.Html ()
template = renderBlogSection

-- | Blog posts table section
renderBlogSection :: Maybe Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderBlogSection selectedShow blogPosts =
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
    case blogPosts of
      [] ->
        Lucid.div_ [Lucid.class_ $ cls [Tokens.textGray600, "text-center", Tokens.p8]] $ do
          Lucid.p_ "No blog posts yet."
          Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, "mt-2"]] "Share your thoughts with your audience!"
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
