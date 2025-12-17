{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Delete.Templates.ErrorBanner
  ( renderErrorBannerWithCard,
    emptyResponse,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks, showEpisodesLinks)
import API.Types
import Component.Banner (BannerType (..), renderBanner)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPushUrl_, hxSwap_, hxTarget_)
import Lucid.Responsive (cls)
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
  renderBanner Error "Archive Failed" errorMsg

-- | Render the episode card (copied from Episode template to avoid circular dependency)
renderEpisodeCard :: Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeCard showModel episode = do
  let episodeId = episode.id
      episodeCardId = [i|episode-card-#{episodeId}|]
      episodeEditUrl = Links.linkURI $ dashboardEpisodesLinks.editGet showModel.slug episode.id episode.slug
      episodeDelUrl = Links.linkURI $ showEpisodesLinks.delete showModel.slug episode.id episode.slug
  Lucid.div_ [Lucid.class_ $ cls ["border", "border-gray-300", Tokens.p4], Lucid.id_ episodeCardId] $ do
    Lucid.div_ [Lucid.class_ $ cls ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ Tokens.fontBold] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600]] $ do
          case episode.scheduledAt of
            Just scheduledAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %l:%M %p" scheduledAt
            Nothing -> "Not scheduled"
          " • "
          "Duration TBD"
      Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap2]] $ do
        Lucid.a_ [Lucid.href_ [i|/#{episodeEditUrl}|], hxGet_ [i|/#{episodeEditUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ $ cls ["bg-gray-600", Tokens.textWhite, Tokens.px3, "py-1", Tokens.textXs, Tokens.fontBold, "hover:bg-gray-700", "no-underline"]] "EDIT"
        Lucid.button_
          [ hxDelete_ [i|/#{episodeDelUrl}|],
            hxTarget_ ("#" <> episodeCardId),
            hxSwap_ "outerHTML",
            LucidBase.makeAttributes "hx-confirm" "Are you sure you want to archive this episode? It will no longer appear in your dashboard or public show page.",
            Lucid.class_ $ cls ["bg-red-600", Tokens.textWhite, Tokens.px3, "py-1", Tokens.textXs, Tokens.fontBold, "hover:bg-red-700"]
          ]
          "ARCHIVE"

    Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray700, "mb-3"]] $ do
      case episode.description of
        Nothing -> "No description available"
        Just desc -> do
          Lucid.toHtml $ Text.take 150 desc
          if Text.length desc > 150 then "..." else ""

    Lucid.div_ [Lucid.class_ $ cls ["flex", "justify-between", "items-center", Tokens.textXs, "text-gray-500"]] $ do
      Lucid.div_ $ do
        "Status: "
        Lucid.span_ [Lucid.class_ $ cls ["text-green-600", Tokens.fontBold]] "Published"
      Lucid.div_ "- views • - downloads"

-- | Empty response for successful deletes (episode card is removed by hx-target)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
