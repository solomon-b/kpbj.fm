{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Event.Get.Templates.Page
  ( template,
    notFoundTemplate,
    renderEventDescription,
    renderTag,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, eventsLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContent)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Lucid.Responsive (cls, lg, md)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsLinks.list Nothing Nothing

eventsGetTagUrl :: Text -> Links.URI
eventsGetTagUrl tagName = Links.linkURI $ eventsLinks.list (Just tagName) Nothing

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Render event description as markdown
renderEventDescription :: Text -> Lucid.Html ()
renderEventDescription = renderContent

-- | Render a single tag
renderTag :: EventTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.a_
    [ Lucid.href_ [i|/#{eventsGetTagUrl (EventTags.etmName tag)}|],
      hxGet_ [i|/#{eventsGetTagUrl (EventTags.etmName tag)}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ $ cls ["bg-gray-200", Tokens.textGray800, "px-2", "py-1", Tokens.textSm, "font-mono", "hover:bg-gray-300", "no-underline"]
    ]
    $ Lucid.toHtml ("#" <> tag.etmName)

-- | Main event template
template :: Events.Model -> [EventTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template event eventTags author = do
  -- Event Header Section
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8]] $ do
    Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-1", lg "grid-cols-4", Tokens.gap8]] $ do
      -- Event Poster Image
      Lucid.div_ [Lucid.class_ $ lg "col-span-1"] $ do
        case event.emPosterImageUrl of
          Just posterUrl ->
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{posterUrl}|],
                Lucid.alt_ (event.emTitle <> " poster"),
                Lucid.class_ $ cls [Tokens.fullWidth, "aspect-square", "object-cover", Tokens.border2, "border-gray-600"]
              ]
          Nothing ->
            Lucid.div_
              [Lucid.class_ $ cls [Tokens.fullWidth, "aspect-square", "bg-gray-300", Tokens.border2, "border-gray-600", "flex", "items-center", "justify-center", Tokens.textLg]]
              "[NO POSTER IMAGE]"

      -- Event Header Info
      Lucid.div_ [Lucid.class_ $ cls [lg "col-span-3", "flex", "flex-col"]] $ do
        -- Title
        Lucid.h1_ [Lucid.class_ $ cls [Tokens.text3xl, Tokens.fontBold, Tokens.mb6]] $
          Lucid.toHtml event.emTitle

        -- Key Details
        Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-1", md "grid-cols-2", Tokens.gap6, Tokens.mb6]] $ do
          -- Date and Time
          Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", "gap-3"]] $ do
            Lucid.span_ [Lucid.class_ Tokens.text2xl] "📅"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ Tokens.fontBold] "DATE & TIME"
              Lucid.div_ [Lucid.class_ Tokens.textGray600] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
              Lucid.div_ [Lucid.class_ Tokens.textGray600] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                    <> " - "
                    <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt

          -- Location
          Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", "gap-3"]] $ do
            Lucid.span_ [Lucid.class_ Tokens.text2xl] "📍"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ Tokens.fontBold] "LOCATION"
              Lucid.div_ [Lucid.class_ Tokens.textGray600] $ Lucid.toHtml event.emLocationName
              Lucid.div_ [Lucid.class_ Tokens.textGray600] $ Lucid.toHtml event.emLocationAddress

        -- Tags (pushed to bottom right)
        Lucid.div_ [Lucid.class_ $ cls ["flex", "justify-end", "mt-auto"]] $ do
          Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-wrap", Tokens.gap2]] $ do
            mapM_ renderTag eventTags

  -- Event Description Section (Full Width)
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8]] $ do
    renderEventDescription event.emDescription

    -- Event creator info
    Lucid.div_ [Lucid.class_ $ cls ["mt-6", Tokens.p4, "bg-gray-50", "border-l-4", "border-gray-800"]] $ do
      Lucid.div_ [Lucid.class_ $ cls ["flex", "items-start", Tokens.gap4]] $ do
        Lucid.div_ [Lucid.class_ $ cls ["w-12", "h-12", "bg-gray-300", "rounded-full", "flex", "items-center", "justify-center", Tokens.textSm]] $
          Lucid.toHtml $
            Text.take 2 (display author.mDisplayName)
        Lucid.div_ $ do
          Lucid.h3_ [Lucid.class_ $ cls [Tokens.fontBold, "mb-1"]] $
            Lucid.toHtml ("Event created by " <> display author.mDisplayName)
          Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600]] $
            Lucid.toHtml $
              "KPBJ " <> Text.pack (show author.mUserRole) <> " • " <> display author.mFullName

  -- Navigation back to events
  Lucid.div_ [Lucid.class_ "text-center"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
      ]
      "← BACK TO EVENTS"

-- | Template for when event is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h1_ [Lucid.class_ $ cls [Tokens.text3xl, Tokens.fontBold, Tokens.mb4]] "Event Not Found"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray600, Tokens.mb6]] $ do
      "The event with slug \""
      Lucid.code_ [Lucid.class_ $ cls [Tokens.bgGray100, "px-2", "py-1"]] $ Lucid.toHtml (display slug)
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
      ]
      "← BACK TO EVENTS"
