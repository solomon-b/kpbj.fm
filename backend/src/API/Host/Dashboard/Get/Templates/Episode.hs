{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Episode
  ( renderEpisodeCard,
  )
where

import {-# SOURCE #-} API (episodeEditGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Effects.Database.Tables.Episode qualified as Episode
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Render individual episode card
renderEpisodeCard :: Episode.EpisodeModel -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "border border-gray-300 p-4"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start mb-2"] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ "font-bold"] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
          Lucid.toHtml (show episode.createdAt)
          " • "
          -- TODO: Add duration when available
          "Duration TBD"
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        let episodeEditUrl = Links.linkURI $ episodeEditGetLink episode.id
        Lucid.a_ [Lucid.href_ [i|/#{episodeEditUrl}|], hxGet_ [i|/#{episodeEditUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "bg-gray-600 text-white px-3 py-1 text-xs font-bold hover:bg-gray-700 no-underline"] "EDIT"
        Lucid.button_ [Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"] "DELETE"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-700 mb-3"] $ do
      case episode.description of
        Nothing -> "No description available"
        Just desc -> do
          Lucid.toHtml $ Text.take 150 desc
          if Text.length desc > 150 then "..." else ""

    Lucid.div_ [Lucid.class_ "flex justify-between items-center text-xs text-gray-500"] $ do
      Lucid.div_ $ do
        "Status: "
        Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "Published"
      Lucid.div_ "👀 - views • 🎧 - downloads" -- TODO: Add real stats
