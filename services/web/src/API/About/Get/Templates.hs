module API.About.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.PageHeader (pageHeader)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens (fgPrimary)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Markdown (defaultMarkdownConfig, renderMarkdownPure)
import Lucid qualified
import Lucid.Base (makeAttributes)

--------------------------------------------------------------------------------

-- | About page template
template :: Maybe SitePages.Model -> Lucid.Html ()
template mPage = do
  let title = maybe "About KPBJ" SitePages.spmTitle mPage
      content = maybe fallbackContent SitePages.spmContent mPage
  pageHeader title
  Lucid.div_ [class_ $ base ["prose", "prose-lg", fgPrimary, "max-w-none"]] $ do
    case renderMarkdownPure defaultMarkdownConfig content of
      Left _ -> Lucid.p_ "Content could not be rendered."
      Right html -> html
  newsletterSignup

-- | Newsletter signup section (matches homepage styling)
newsletterSignup :: Lucid.Html ()
newsletterSignup = do
  Lucid.section_ [Lucid.class_ "mt-auto mb-4 text-center"] $ do
    Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-4"] "Stay in the Loop"

    Lucid.iframe_ [Lucid.name_ "hidden_iframe_about", Lucid.id_ "hidden_iframe_about", Lucid.style_ "display:none;"] ""

    Lucid.form_ [Lucid.action_ "https://docs.google.com/forms/u/0/d/e/1FAIpQLSfeM91iQ_A7ybaa070b8jiznHNRIJ_2JU0F7wJjo7vAvkS3tQ/formResponse", Lucid.method_ "POST", Lucid.target_ "hidden_iframe_about", Lucid.onsubmit_ "handleAboutSubmit(event)", Lucid.class_ "w-full max-w-sm"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center border-b border-[var(--theme-border)] py-2"] $ do
        Lucid.input_ [Lucid.type_ "email", Lucid.name_ "entry.936311333", Lucid.required_ "", Lucid.placeholder_ "you@example.com", makeAttributes "aria-label" "Email Address", Lucid.class_ "appearance-none bg-transparent border-none w-full text-[var(--theme-fg)] mr-3 py-1 px-2 leading-tight focus:outline-none"]
        Lucid.button_ [Lucid.type_ "submit", Lucid.id_ "about-subscribe-button", Lucid.class_ "px-3 py-2 text-xs font-medium text-center inline-flex items-center text-[var(--theme-fg-inverse)] bg-[var(--theme-bg-inverse)] rounded-md hover:bg-[var(--theme-hover-bg)] focus:ring-4 focus:outline-none focus:ring-[var(--theme-border)]"] "Subscribe"
      Lucid.p_ [Lucid.id_ "about-form-confirmation", Lucid.class_ "mt-2 text-[var(--theme-success-text)]", Lucid.style_ "display: none;"] "Thanks for subscribing!"

    Lucid.script_ "function handleAboutSubmit(event) { const form = event.target; const confirmation = form.querySelector('#about-form-confirmation'); const button = form.querySelector('#about-subscribe-button'); button.disabled = true; button.innerText = 'Submitting...'; setTimeout(() => { confirmation.style.display = 'block'; form.reset(); button.disabled = false; button.innerText = 'Subscribe'; }, 500); }"

-- | Fallback content if page is not found in database
fallbackContent :: Text
fallbackContent = "KPBJ is an independent, community-driven radio station based in Shadow Hills, California."
