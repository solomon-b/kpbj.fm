module API.Donate.Get.Templates
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

--------------------------------------------------------------------------------

-- | Donate page template with dynamic content and PayPal integration.
template :: Maybe SitePages.Model -> Lucid.Html ()
template mPage = do
  let title = maybe "Support KPBJ" SitePages.spmTitle mPage
      content = maybe fallbackContent SitePages.spmContent mPage
  pageHeader title
  Lucid.div_ [class_ $ base ["prose", "prose-lg", fgPrimary, "max-w-none"]] $ do
    case renderMarkdownPure defaultMarkdownConfig content of
      Left _ -> Lucid.p_ "Content could not be rendered."
      Right html -> html
  paypalWidget

-- | PayPal donation button widget.
paypalWidget :: Lucid.Html ()
paypalWidget = do
  Lucid.div_ [Lucid.id_ "paypal-container-YBRDJJA6AGBL6", Lucid.class_ "w-full max-w-96 mt-8"] ""
  Lucid.script_ [] paypalScript

-- | PayPal SDK loader script.
paypalScript :: Text
paypalScript = "if (typeof paypal === 'undefined') { const script = document.createElement('script'); script.src = 'https://www.paypal.com/sdk/js?client-id=BAA56A9gcLxsoKcdDF1ipwRFhXfp8nondkp4mIJClY5tiW5pFR9C1kMlHNKYFrRYTeecQ-MzTmxkjAkFFQ&components=hosted-buttons&enable-funding=venmo&currency=USD'; script.onload = function() { paypal.HostedButtons({ hostedButtonId: 'YBRDJJA6AGBL6' }).render('#paypal-container-YBRDJJA6AGBL6'); }; document.head.appendChild(script); } else { paypal.HostedButtons({ hostedButtonId: 'YBRDJJA6AGBL6' }).render('#paypal-container-YBRDJJA6AGBL6'); }"

-- | Fallback content if page is not found in database.
fallbackContent :: Text
fallbackContent = "KPBJ is an independent, community-driven radio station. Your support helps keep us on the air."
