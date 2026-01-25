module API.About.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.PageHeader (pageHeader)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens (textGray800)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Markdown (defaultMarkdownConfig, renderMarkdownPure)
import Lucid qualified

--------------------------------------------------------------------------------

-- | About page template
template :: Maybe SitePages.Model -> Lucid.Html ()
template mPage = do
  let title = maybe "About KPBJ" SitePages.spmTitle mPage
      content = maybe fallbackContent SitePages.spmContent mPage
  pageHeader title
  Lucid.div_ [class_ $ base ["prose", "prose-lg", textGray800, "max-w-none"]] $ do
    case renderMarkdownPure defaultMarkdownConfig content of
      Left _ -> Lucid.p_ "Content could not be rendered."
      Right html -> html

-- | Fallback content if page is not found in database
fallbackContent :: Text
fallbackContent = "KPBJ is an independent, community-driven radio station based in Shadow Hills, California."
