{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Delete.Templates.ErrorBanner
  ( renderErrorBannerWithCard,
    emptyResponse,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesDeleteLink, episodesEditGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPushUrl_, hxSwap_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render an error banner AND the episode card (to prevent card removal on error)
-- The error banner uses hx-swap-oob to inject at the top
-- The episode card is returned as the main response to replace itself (no change)
renderErrorBannerWithCard :: Shows.Model -> Episodes.Model -> Text -> Lucid.Html ()
renderErrorBannerWithCard showModel episode errorMsg = do
  -- Main response: return the episode card unchanged
  renderEpisodeCard showModel episode
  -- Out-of-band: inject error banner at top
  renderErrorBanner errorMsg

-- | Render the error banner with hx-swap-oob
renderErrorBanner :: Text -> Lucid.Html ()
renderErrorBanner errorMsg = do
  Lucid.div_
    [ Lucid.id_ "error-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true",
      Lucid.class_ "w-full"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "error-banner",
          Lucid.class_ "bg-red-100 border-2 border-red-600 p-4 mb-6"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "⚠️"
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Archive Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#error-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              "✕"

-- | Render the episode card (copied from Episode template to avoid circular dependency)
renderEpisodeCard :: Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeCard showModel episode = do
  let episodeId = episode.id
      episodeCardId = [i|episode-card-#{episodeId}|]
      episodeEditUrl = Links.linkURI $ episodesEditGetLink showModel.slug episode.slug
      episodeDelUrl = Links.linkURI $ episodesDeleteLink showModel.slug episode.slug
  Lucid.div_ [Lucid.class_ "border border-gray-300 p-4", Lucid.id_ episodeCardId] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start mb-2"] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ "font-bold"] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
          case episode.scheduledAt of
            Just scheduledAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %l:%M %p" scheduledAt
            Nothing -> "Not scheduled"
          " • "
          "Duration TBD"
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.a_ [Lucid.href_ [i|/#{episodeEditUrl}|], hxGet_ [i|/#{episodeEditUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "bg-gray-600 text-white px-3 py-1 text-xs font-bold hover:bg-gray-700 no-underline"] "EDIT"
        Lucid.button_
          [ hxDelete_ [i|/#{episodeDelUrl}|],
            hxTarget_ ("#" <> episodeCardId),
            hxSwap_ "outerHTML",
            LucidBase.makeAttributes "hx-confirm" "Are you sure you want to archive this episode? It will no longer appear in your dashboard or public show page.",
            Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"
          ]
          "ARCHIVE"

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
      Lucid.div_ "👀 - views • 🎧 - downloads"

-- | Empty response for successful deletes (episode card is removed by hx-target)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
