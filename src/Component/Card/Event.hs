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

import API.Links (apiLinks, eventsLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Effects.Markdown (renderContent)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
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

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------
-- Main Render Functions

-- | Render an event card with the specified variant.
renderEventCard :: Variant -> Events.Model -> Lucid.Html ()
renderEventCard variant event = do
  Lucid.article_ [class_ $ base [Tokens.bgWhite, "dark:bg-gray-800"]] $ do
    case variant of
      Summary -> do
        -- Vertical stack layout for summary
        renderPosterImage variant event
        Lucid.div_ [class_ $ base ["mt-2"]] $ do
          renderTitle variant event
          renderDateAndLocation variant event
      Detail -> do
        -- Vertical stack layout for detail view (centered on desktop)
        Lucid.div_ [class_ $ do { base []; desktop ["max-w-lg", "mx-auto"] }] $ do
          renderPosterImage variant event
          Lucid.div_ [class_ $ base ["mt-4"]] $ do
            renderTitle variant event
            renderDateAndLocation variant event
            renderDescription event

-- | Convenience function for summary cards (list view).
renderEventCardSummary :: Events.Model -> Lucid.Html ()
renderEventCardSummary = renderEventCard Summary

-- | Convenience function for detail cards (detail page).
renderEventCardDetail :: Events.Model -> Lucid.Html ()
renderEventCardDetail = renderEventCard Detail

--------------------------------------------------------------------------------
-- Component Functions

-- | Render event poster image or placeholder.
renderPosterImage :: Variant -> Events.Model -> Lucid.Html ()
renderPosterImage variant event = do
  let eventUrl = eventGetUrl event.emId event.emSlug
  case variant of
    Summary ->
      Lucid.a_
        [ Lucid.href_ [i|/#{eventUrl}|],
          hxGet_ [i|/#{eventUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        $ renderImage event
    Detail -> renderImage event

-- | Render the actual image element.
renderImage :: Events.Model -> Lucid.Html ()
renderImage event =
  case event.emPosterImageUrl of
    Just posterUrl ->
      Lucid.img_
        [ Lucid.src_ [i|/#{mediaGetUrl}/#{posterUrl}|],
          Lucid.alt_ (event.emTitle <> " poster"),
          class_ $ base [Tokens.fullWidth, "aspect-square", "object-cover", "border", "border-gray-300"]
        ]
    Nothing ->
      Lucid.div_
        [class_ $ base [Tokens.fullWidth, "aspect-square", "bg-gray-300", "dark:bg-gray-700", "flex", "items-center", "justify-center", Tokens.textLg, "border", "border-gray-300"]]
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
  Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap4, Tokens.textSm]; tablet ["grid-cols-2"] }] $ do
    -- Date and time
    Lucid.div_ $ do
      case variant of
        Summary -> pure ()
        Detail -> Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textGray800, "dark:text-gray-200"]] "DATE & TIME"
      Lucid.div_ [Lucid.class_ [i|#{Tokens.textGray600} dark:text-gray-400|]] $
        Lucid.toHtml $
          formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
      Lucid.div_ [Lucid.class_ [i|#{Tokens.textGray600} dark:text-gray-400|]] $
        Lucid.toHtml $
          formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
            <> " - "
            <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt

    -- Location (detail view only)
    case variant of
      Summary -> pure ()
      Detail ->
        Lucid.div_ $ do
          Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textGray800, "dark:text-gray-200"]] "LOCATION"
          Lucid.div_ [Lucid.class_ [i|#{Tokens.textGray600} dark:text-gray-400|]] $ Lucid.toHtml event.emLocationName
          Lucid.div_ [Lucid.class_ [i|#{Tokens.textGray600} dark:text-gray-400|]] $ Lucid.toHtml event.emLocationAddress

-- | Render event description as markdown.
renderDescription :: Events.Model -> Lucid.Html ()
renderDescription event =
  Lucid.div_ [class_ $ base [Tokens.mt4]] $ do
    renderContent event.emDescription
