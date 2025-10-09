{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.List
  ( renderListContent,
    renderEventCard,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink)
import Data.Foldable (traverse_)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventGetUrl :: Text.Text -> Links.URI
eventGetUrl slug = Links.linkURI $ eventGetLink slug

--------------------------------------------------------------------------------

-- | Render the events listing content
renderListContent :: [Events.Model] -> Lucid.Html ()
renderListContent events = do
  Lucid.div_ [Lucid.class_ "space-y-8"] $ do
    Lucid.section_ [Lucid.class_ "space-y-6"] $ do
      if null events
        then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Events Scheduled"
          Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for upcoming community events!"
        else traverse_ renderEventCard events

-- | Render an event card for the list view
renderEventCard :: Events.Model -> Lucid.Html ()
renderEventCard event = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-6"] $ do
      -- Event image placeholder and type badge
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_
          [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg mb-4"]
          "[EVENT IMAGE]"
        Lucid.div_ [Lucid.class_ "text-center"] $ do
          Lucid.div_
            [Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-bold"]
            "EVENT"

      -- Event details
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        -- Title
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "hover:underline"
            ]
            $ Lucid.toHtml event.emTitle

        -- Date and location info
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-4 text-sm mb-4"] $ do
          Lucid.div_ $ do
            Lucid.div_ [Lucid.class_ "font-bold text-gray-800"] "📅 DATE & TIME"
            Lucid.div_ [Lucid.class_ "text-gray-600"] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
            Lucid.div_ [Lucid.class_ "text-gray-600"] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                  <> " - "
                  <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt
          Lucid.div_ $ do
            Lucid.div_ [Lucid.class_ "font-bold text-gray-800"] "📍 LOCATION"
            Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationName
            Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationAddress

        -- Description preview
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $ do
          let truncatedDescription = Text.take 200 event.emDescription
          Lucid.toHtml $
            truncatedDescription <> if Text.length event.emDescription > 200 then "..." else ""

        -- View Event button
        Lucid.div_ [Lucid.class_ "flex items-center gap-4"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700"
            ]
            "VIEW EVENT"
