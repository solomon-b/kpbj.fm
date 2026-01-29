module API.PrivacyPolicy.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Markdown (defaultMarkdownConfig, renderMarkdownPure)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Privacy policy page template
template :: Maybe SitePages.Model -> Lucid.Html ()
template mPage = do
  let content = maybe fallbackContent SitePages.spmContent mPage
  Lucid.div_ [Lucid.class_ "prose prose-lg text-[var(--theme-fg)] max-w-4xl"] $ do
    case renderMarkdownPure defaultMarkdownConfig content of
      Left _ -> Lucid.p_ "Content could not be rendered."
      Right html -> html

-- | Fallback content if page is not found in database
fallbackContent :: Text
fallbackContent = "# Privacy Policy\n\nKPBJ-FM is committed to protecting your privacy."
