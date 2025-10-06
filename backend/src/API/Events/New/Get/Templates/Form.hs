{-# LANGUAGE QuasiQuotes #-}

module API.Events.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink, eventsNewPostLink)
import Data.String.Interpolate (i)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventsNewPostUrl :: Links.URI
eventsNewPostUrl = Links.linkURI eventsNewPostLink

--------------------------------------------------------------------------------

-- | New event form template
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "NEW EVENT"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Organizer: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetUrl}|],
            hxGet_ [i|/#{eventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW EVENTS"

  -- New Event Form
  Lucid.form_ [Lucid.action_ [i|/#{eventsNewPostUrl}|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Event Details
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EVENT DETAILS"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. KPBJ Spring Concert"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Tags"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "tags",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "live-music, concert, fundraiser, community"
            ]
          Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Comma separated tags"

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Description *"
          Lucid.textarea_
            [ Lucid.name_ "description",
              Lucid.required_ "true",
              Lucid.rows_ "8",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
              Lucid.placeholder_ "Describe your event. Include any special details, what attendees should expect, what to bring, etc."
            ]
            ""

    -- Date and Time
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "DATE & TIME"

      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Start Date & Time *"
          Lucid.input_
            [ Lucid.type_ "datetime-local",
              Lucid.name_ "starts_at",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "End Date & Time *"
          Lucid.input_
            [ Lucid.type_ "datetime-local",
              Lucid.name_ "ends_at",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
            ]

    -- Location
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "LOCATION"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Venue Name *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "location_name",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Shadow Hills Community Center"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Address *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "location_address",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. 1247 Underground Ave, Shadow Hills, CA"
            ]

    -- Publishing Options
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "PUBLISHING"

      Lucid.div_ [Lucid.class_ "space-y-4"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "flex items-center gap-2"] $ do
            Lucid.input_ [Lucid.type_ "radio", Lucid.name_ "status", Lucid.value_ "draft", Lucid.checked_]
            Lucid.span_ [Lucid.class_ "font-bold"] "Save as Draft"
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600 ml-6"] "Keep private until you're ready to publish"

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "flex items-center gap-2"] $ do
            Lucid.input_ [Lucid.type_ "radio", Lucid.name_ "status", Lucid.value_ "published"]
            Lucid.span_ [Lucid.class_ "font-bold"] "Publish Immediately"
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600 ml-6"] "Make visible to the public right away"

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
          ]
          "CREATE EVENT"
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetUrl}|],
            hxGet_ [i|/#{eventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
          ]
          "CANCEL"
