{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks)
import API.Types (EventsRoutes (..))
import Data.String.Interpolate (i)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Homepage template with newsletter signup
template :: StorageBackend -> Maybe Events.Model -> Lucid.Html ()
template backend mFeaturedEvent = do
  Lucid.div_ [Lucid.class_ "max-w-lg mx-auto"] $
    case mFeaturedEvent of
      Just event
        | Just posterUrl <- event.emPosterImageUrl ->
            let eventUrl = Links.linkURI $ eventsLinks.detailWithSlug event.emId event.emSlug
             in Lucid.a_ [Lucid.href_ [i|/#{eventUrl}|], hxGet_ [i|/#{eventUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true"] $ do
                  Lucid.img_ [Lucid.src_ (buildMediaUrl backend posterUrl), Lucid.alt_ (event.emTitle <> " event flyer"), Lucid.class_ "w-full"]
      _ ->
        Lucid.a_ [Lucid.href_ "https://fccdata.org/?call=kpbj&facid=&city=&state=&ccode=1&country=US"] $ do
          Lucid.img_ [Lucid.src_ "/static/range.png", Lucid.class_ "w-full"]

  Lucid.section_ [Lucid.class_ "mt-auto mb-4 text-center"] $ do
    Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-4"] "Stay in the Loop"

    Lucid.iframe_ [Lucid.name_ "hidden_iframe", Lucid.id_ "hidden_iframe", Lucid.style_ "display:none;"] ""

    Lucid.form_ [Lucid.action_ "https://docs.google.com/forms/u/0/d/e/1FAIpQLSfeM91iQ_A7ybaa070b8jiznHNRIJ_2JU0F7wJjo7vAvkS3tQ/formResponse", Lucid.method_ "POST", Lucid.target_ "hidden_iframe", Lucid.onsubmit_ "handleSubmit(event)", Lucid.class_ "w-full max-w-sm"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center border-b border-[var(--theme-border)] py-2"] $ do
        Lucid.input_ [Lucid.type_ "email", Lucid.name_ "entry.936311333", Lucid.required_ "", Lucid.placeholder_ "you@example.com", makeAttributes "aria-label" "Email Address", Lucid.class_ "appearance-none bg-transparent border-none w-full text-[var(--theme-fg)] mr-3 py-1 px-2 leading-tight focus:outline-none"]
        Lucid.button_ [Lucid.type_ "submit", Lucid.id_ "subscribe-button", Lucid.class_ "px-3 py-2 text-xs font-medium text-center inline-flex items-center text-[var(--theme-fg-inverse)] bg-[var(--theme-bg-inverse)] rounded-md hover:bg-[var(--theme-hover-bg)] focus:ring-4 focus:outline-none focus:ring-[var(--theme-border)]"] $ do
          "Subscribe"
      Lucid.p_ [Lucid.id_ "form-confirmation", Lucid.class_ "mt-2 text-[var(--theme-success-text)]", Lucid.style_ "display: none;"] "Thanks for subscribing!"
  Lucid.script_ "function handleSubmit(event) { const form = event.target; const confirmation = form.querySelector('#form-confirmation'); const button = form.querySelector('#subscribe-button'); button.disabled = true; button.innerText = 'Submitting...'; setTimeout(() => { confirmation.style.display = 'block'; form.reset(); button.disabled = false; button.innerText = 'Subscribe'; }, 500); }"
