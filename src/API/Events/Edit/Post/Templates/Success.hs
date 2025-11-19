{-# LANGUAGE QuasiQuotes #-}

module API.Events.Edit.Post.Templates.Success
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import Data.String.Interpolate (i)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventGetLink eventId slug

--------------------------------------------------------------------------------

template :: Events.Model -> Lucid.Html ()
template event = do
  let eventUrl = eventGetUrl (Events.emId event) (Events.emSlug event)
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Event Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your event has been updated and saved."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{eventUrl}|],
          hxGet_ [i|/#{eventUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "VIEW EVENT"
      Lucid.a_
        [ Lucid.href_ [i|/#{eventsGetUrl}|],
          hxGet_ [i|/#{eventsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-6 py-3 font-bold hover:bg-gray-500"
        ]
        "BACK TO EVENTS"
