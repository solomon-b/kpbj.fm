{-# LANGUAGE QuasiQuotes #-}

module API.Events.Event.Get.Templates.Page
  ( template,
    notFoundTemplate,
    renderEventDescription,
    renderTag,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink, mediaGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventsGetTagUrl :: Text -> Links.URI
eventsGetTagUrl tagName = Links.linkURI $ eventsGetLink (Just tagName) Nothing

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

--------------------------------------------------------------------------------

-- | Render event description with simple paragraph breaks
renderEventDescription :: Text -> Lucid.Html ()
renderEventDescription description = do
  let paragraphs = Text.splitOn "\n\n" description
  Lucid.div_ [Lucid.class_ "prose max-w-none text-gray-700 leading-relaxed space-y-4"] $ do
    mapM_ renderParagraph paragraphs
  where
    renderParagraph para =
      if Text.null (Text.strip para)
        then pure ()
        else Lucid.p_ $ Lucid.toHtml para

-- | Render a single tag
renderTag :: EventTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.a_
    [ Lucid.href_ [i|/#{eventsGetTagUrl (EventTags.etmName tag)}|],
      hxGet_ [i|/#{eventsGetTagUrl (EventTags.etmName tag)}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-sm font-mono hover:bg-gray-300 no-underline"
    ]
    $ Lucid.toHtml ("#" <> tag.etmName)

-- | Main event template
template :: Events.Model -> [EventTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template event eventTags author = do
  -- Event Header Section
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Event Poster Image
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        case event.emPosterImageUrl of
          Just posterUrl ->
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{posterUrl}|],
                Lucid.alt_ (event.emTitle <> " poster"),
                Lucid.class_ "w-full aspect-square object-cover border-2 border-gray-600"
              ]
          Nothing ->
            Lucid.div_
              [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"]
              "[NO POSTER IMAGE]"

      -- Event Header Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3 flex flex-col"] $ do
        -- Title
        Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-6"] $
          Lucid.toHtml event.emTitle

        -- Key Details
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6 mb-6"] $ do
          -- Date and Time
          Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
            Lucid.span_ [Lucid.class_ "text-2xl"] "📅"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ "font-bold"] "DATE & TIME"
              Lucid.div_ [Lucid.class_ "text-gray-600"] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
              Lucid.div_ [Lucid.class_ "text-gray-600"] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                    <> " - "
                    <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt

          -- Location
          Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
            Lucid.span_ [Lucid.class_ "text-2xl"] "📍"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ "font-bold"] "LOCATION"
              Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationName
              Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationAddress

        -- Tags (pushed to bottom right)
        Lucid.div_ [Lucid.class_ "flex justify-end mt-auto"] $ do
          Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2"] $ do
            mapM_ renderTag eventTags

  -- Event Description Section (Full Width)
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    renderEventDescription event.emDescription

    -- Event creator info
    Lucid.div_ [Lucid.class_ "mt-6 p-4 bg-gray-50 border-l-4 border-gray-800"] $ do
      Lucid.div_ [Lucid.class_ "flex items-start gap-4"] $ do
        Lucid.div_ [Lucid.class_ "w-12 h-12 bg-gray-300 rounded-full flex items-center justify-center text-sm"] $
          Lucid.toHtml $
            Text.take 2 (display author.mDisplayName)
        Lucid.div_ $ do
          Lucid.h3_ [Lucid.class_ "font-bold mb-1"] $
            Lucid.toHtml ("Event created by " <> display author.mDisplayName)
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] $
            Lucid.toHtml $
              "KPBJ " <> Text.pack (show author.mUserRole) <> " • " <> display author.mFullName

  -- Navigation back to events
  Lucid.div_ [Lucid.class_ "text-center"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO EVENTS"

-- | Template for when event is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "Event Not Found"
    Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] $ do
      "The event with slug \""
      Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ Lucid.toHtml (display slug)
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO EVENTS"
