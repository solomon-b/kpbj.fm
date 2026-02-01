module API.Donate.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.PageHeader (pageHeader)
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Donate page template with PayPal integration
template :: Lucid.Html ()
template = do
  pageHeader "SUPPORT KPBJ"
  Lucid.div_ [Lucid.id_ "paypal-container-YBRDJJA6AGBL6", Lucid.class_ "w-full max-w-96"] ""
  -- Load PayPal SDK if not already loaded
  Lucid.script_ [] ("if (typeof paypal === 'undefined') { const script = document.createElement('script'); script.src = 'https://www.paypal.com/sdk/js?client-id=BAA56A9gcLxsoKcdDF1ipwRFhXfp8nondkp4mIJClY5tiW5pFR9C1kMlHNKYFrRYTeecQ-MzTmxkjAkFFQ&components=hosted-buttons&enable-funding=venmo&currency=USD'; script.onload = function() { paypal.HostedButtons({ hostedButtonId: 'YBRDJJA6AGBL6' }).render('#paypal-container-YBRDJJA6AGBL6'); }; document.head.appendChild(script); } else { paypal.HostedButtons({ hostedButtonId: 'YBRDJJA6AGBL6' }).render('#paypal-container-YBRDJJA6AGBL6'); }" :: Text)
