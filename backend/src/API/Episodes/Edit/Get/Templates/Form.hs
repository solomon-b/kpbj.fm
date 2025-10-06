{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Episodes.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

--------------------------------------------------------------------------------

-- | Episode edit template
template :: Episode.EpisodeModel -> Show.ShowModel -> UserMetadata.Model -> Lucid.Html ()
template episode s userMeta = do
  let episodeId = episode.id
  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml s.title
          " • "
          Lucid.strong_ "Host: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO DASHBOARD"

  -- Edit Episode Form
  Lucid.form_ [Lucid.action_ [i|/episodes/#{episodeId}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Episode Details
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EPISODE DETAILS"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.value_ episode.title,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Industrial Depths #087"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Number"
          Lucid.input_
            [ Lucid.type_ "number",
              Lucid.name_ "episode_number",
              Lucid.value_ (display episode.episodeNumber),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "87"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Season Number"
          Lucid.input_
            [ Lucid.type_ "number",
              Lucid.name_ "season_number",
              Lucid.value_ (display episode.seasonNumber),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "1"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Description"
          Lucid.textarea_
            [ Lucid.name_ "description",
              Lucid.rows_ "6",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
              Lucid.placeholder_ "Describe this episode. What music was featured? Any special guests or themes?"
            ]
            (maybe "" Lucid.toHtml episode.description)

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
          ]
          "UPDATE EPISODE"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
          ]
          "CANCEL"
