{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks, rootLink, staticAssetLink)
import API.Newsletter.Subscribe.Post.Templates qualified as Newsletter
import API.Types (EventsRoutes (..))
import Data.String.Interpolate (i)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
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
          Lucid.img_ [Lucid.src_ (rootLink $ staticAssetLink "range.png"), Lucid.class_ "w-full"]

  Lucid.section_ [Lucid.class_ "mt-auto mb-4 text-center flex flex-col items-center"] $ do
    Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-4"] "Stay in the Loop"
    Newsletter.signupForm
