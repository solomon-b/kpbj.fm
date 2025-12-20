{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.Page
  ( template,
    renderEventCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, eventsLinks)
import API.Types
import Component.PageHeader (pageHeader)
import Data.Foldable (traverse_)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventsLinks.detailWithSlug eventId slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Main events page template
template :: [Events.Model] -> Lucid.Html ()
template events = do
  pageHeader "COMMUNITY EVENTS"
  Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
    Lucid.div_ [Lucid.class_ "space-y-8"] $ do
      Lucid.section_ [Lucid.class_ "space-y-6"] $ do
        if null events
          then Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
            Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Events Scheduled"
            Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for upcoming community events!"
          else traverse_ renderEventCard events

-- | Render an event card for the list view
renderEventCard :: Events.Model -> Lucid.Html ()
renderEventCard event = do
  Lucid.article_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb6]] $ do
    Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6]; desktop ["grid-cols-4"] }] $ do
      -- Event poster image
      Lucid.div_ [class_ $ desktop ["col-span-1"]] $ do
        case event.emPosterImageUrl of
          Just posterUrl ->
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{posterUrl}|],
                Lucid.alt_ (event.emTitle <> " poster"),
                class_ $ base [Tokens.fullWidth, "aspect-square", "object-cover", Tokens.border2, "border-gray-600", Tokens.mb4]
              ]
          Nothing ->
            Lucid.div_
              [class_ $ base [Tokens.fullWidth, "aspect-square", "bg-gray-300", Tokens.border2, "border-gray-600", "flex", "items-center", "justify-center", Tokens.textLg, Tokens.mb4]]
              "[NO POSTER]"

      -- Event details
      Lucid.div_ [class_ $ desktop ["col-span-3"]] $ do
        -- Title
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, "mb-3"]] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "hover:underline"
            ]
            $ Lucid.toHtml event.emTitle

        -- Date and location info
        Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap4, Tokens.textSm, Tokens.mb4]; tablet ["grid-cols-2"] }] $ do
          Lucid.div_ $ do
            Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textGray800]] "📅 DATE & TIME"
            Lucid.div_ [Lucid.class_ Tokens.textGray600] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
            Lucid.div_ [Lucid.class_ Tokens.textGray600] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                  <> " - "
                  <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt
          Lucid.div_ $ do
            Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textGray800]] "📍 LOCATION"
            Lucid.div_ [Lucid.class_ Tokens.textGray600] $ Lucid.toHtml event.emLocationName
            Lucid.div_ [Lucid.class_ Tokens.textGray600] $ Lucid.toHtml event.emLocationAddress

        -- Description preview
        Lucid.p_ [class_ $ base ["text-gray-700", Tokens.mb4, "leading-relaxed"]] $ do
          let truncatedDescription = Text.take 200 event.emDescription
          Lucid.toHtml $
            truncatedDescription <> if Text.length event.emDescription > 200 then "..." else ""

        -- View Event button
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ Tokens.buttonPrimary
            ]
            "READ MORE"
