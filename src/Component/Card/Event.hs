{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Event
  ( -- * Card Variants
    Variant (..),

    -- * Render Functions
    renderEventCard,
    renderEventCardSummary,
    renderEventCardDetail,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks)
import API.Types
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Timezone (utcToPacific)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------
-- Types

-- | Card display variant.
data Variant
  = -- | Compact view for list pages (no description/location)
    Summary
  | -- | Full view for detail pages (includes location and description)
    Detail
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- URL Helpers

eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventsLinks.detailWithSlug eventId slug

--------------------------------------------------------------------------------
-- Main Render Functions

-- | Render an event card with the specified variant.
--
-- For the Detail variant, pass pre-rendered description HTML.
-- For the Summary variant, the description parameter is ignored.
renderEventCard :: StorageBackend -> Variant -> Events.Model -> Maybe (Lucid.Html ()) -> Lucid.Html ()
renderEventCard backend variant event mRenderedDescription = do
  Lucid.article_ [class_ $ base [Tokens.bgMain]] $ do
    case variant of
      Summary -> do
        -- Vertical stack layout for summary
        renderPosterImage backend variant event
        Lucid.div_ [class_ $ base ["mt-2"]] $ do
          renderTitle variant event
          renderDateAndLocation variant event
      Detail -> do
        -- Vertical stack layout for detail view (centered on desktop)
        Lucid.div_ [class_ $ do { base []; desktop ["max-w-lg", "mx-auto"] }] $ do
          renderPosterImage backend variant event
          Lucid.div_ [class_ $ base ["mt-4"]] $ do
            renderTitle variant event
            renderDateAndLocation variant event
            renderDescription mRenderedDescription

-- | Convenience function for summary cards (list view).
renderEventCardSummary :: StorageBackend -> Events.Model -> Lucid.Html ()
renderEventCardSummary backend event = renderEventCard backend Summary event Nothing

-- | Convenience function for detail cards (detail page).
renderEventCardDetail :: StorageBackend -> Events.Model -> Lucid.Html () -> Lucid.Html ()
renderEventCardDetail backend event renderedDescription = renderEventCard backend Detail event (Just renderedDescription)

--------------------------------------------------------------------------------
-- Component Functions

-- | Render event poster image or placeholder.
renderPosterImage :: StorageBackend -> Variant -> Events.Model -> Lucid.Html ()
renderPosterImage backend variant event = do
  let eventUrl = eventGetUrl event.emId event.emSlug
  case variant of
    Summary ->
      Lucid.a_
        [ Lucid.href_ [i|/#{eventUrl}|],
          hxGet_ [i|/#{eventUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        $ renderImage backend event
    Detail -> renderImage backend event

-- | Render the actual image element.
renderImage :: StorageBackend -> Events.Model -> Lucid.Html ()
renderImage backend event =
  case event.emPosterImageUrl of
    Just posterUrl ->
      Lucid.img_
        [ Lucid.src_ (buildMediaUrl backend posterUrl),
          Lucid.alt_ (event.emTitle <> " poster"),
          class_ $ base [Tokens.fullWidth, "aspect-[3/4]", "object-cover", "border", Tokens.borderMuted]
        ]
    Nothing ->
      Lucid.div_
        [class_ $ base [Tokens.fullWidth, "aspect-[3/4]", Tokens.bgAlt, "flex", "items-center", "justify-center", Tokens.textLg, "border", Tokens.borderMuted]]
        "[NO POSTER]"

-- | Render event title.
renderTitle :: Variant -> Events.Model -> Lucid.Html ()
renderTitle variant event = do
  let eventUrl = eventGetUrl event.emId event.emSlug
  case variant of
    Summary ->
      Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventUrl}|],
            hxGet_ [i|/#{eventUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml event.emTitle
    Detail ->
      Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb4]] $
        Lucid.toHtml event.emTitle

-- | Render date/time and location information.
renderDateAndLocation :: Variant -> Events.Model -> Lucid.Html ()
renderDateAndLocation variant event =
  let -- Convert UTC to Pacific for display
      startsAtPacific = utcToPacific event.emStartsAt
      endsAtPacific = utcToPacific event.emEndsAt
   in Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap4, Tokens.textSm]; tablet ["grid-cols-2"] }] $ do
        -- Date and time
        Lucid.div_ $ do
          case variant of
            Summary -> pure ()
            Detail -> Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.fgPrimary]] "DATE & TIME"
          Lucid.div_ [Lucid.class_ Tokens.fgMuted] $
            Lucid.toHtml $
              formatTime defaultTimeLocale "%A, %B %d, %Y" startsAtPacific
          Lucid.div_ [Lucid.class_ Tokens.fgMuted] $
            Lucid.toHtml $
              formatTime defaultTimeLocale "%l:%M %p" startsAtPacific
                <> " - "
                <> formatTime defaultTimeLocale "%l:%M %p" endsAtPacific

        -- Location (detail view only)
        case variant of
          Summary -> pure ()
          Detail ->
            Lucid.div_ $ do
              Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.fgPrimary]] "LOCATION"
              Lucid.div_ [Lucid.class_ Tokens.fgMuted] $ Lucid.toHtml event.emLocationName
              Lucid.div_ [Lucid.class_ Tokens.fgMuted] $ Lucid.toHtml event.emLocationAddress

-- | Render event description (pre-rendered HTML).
renderDescription :: Maybe (Lucid.Html ()) -> Lucid.Html ()
renderDescription mRenderedDescription =
  for_ mRenderedDescription (Lucid.div_ [class_ $ base [Tokens.mt4]])
