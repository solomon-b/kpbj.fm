{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StreamSettings.Episodes.Search.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardStreamSettingsLinks)
import API.Types (DashboardStreamSettingsRoutes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors)
import App.Monad (AppM)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Log qualified
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

forceEpisodeUrl :: Links.URI
forceEpisodeUrl = Links.linkURI dashboardStreamSettingsLinks.forceEpisodePost

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/stream-settings/episodes/search?q=...
--
-- Returns an HTML fragment with matching episodes for the force-play search.
handler ::
  Maybe Cookie ->
  Maybe Text ->
  AppM (Lucid.Html ())
handler cookie mQuery =
  handleBannerErrors "Episode search" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can search episodes for force-play." userMetadata

    let query = fromMaybe "" mQuery
    if Text.null (Text.strip query)
      then pure mempty
      else do
        result <- execQuery (Episodes.searchEpisodesWithAudio query)
        case result of
          Left err -> do
            Log.logInfo "Episode search query failed" (Aeson.object ["error" .= show err, "query" .= query])
            pure $ Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] "Error searching episodes."
          Right [] -> pure $ Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] "No episodes found."
          Right episodes -> pure $ renderSearchResults episodes

--------------------------------------------------------------------------------

renderSearchResults :: [Episodes.SearchResult] -> Lucid.Html ()
renderSearchResults results =
  Lucid.div_ [class_ $ base ["flex", "flex-col", "gap-2"]] $
    mapM_ renderResultRow results

renderResultRow :: Episodes.SearchResult -> Lucid.Html ()
renderResultRow sr = do
  let epId = sr.srId
      epNum = sr.srEpisodeNumber
  Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", "gap-4", Tokens.p3, "border", Tokens.border2]] $ do
    Lucid.div_ [class_ $ base ["flex", "flex-col", "gap-1", "min-w-0"]] $ do
      Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textSm, "truncate"]] $
        Lucid.toHtml sr.srShowTitle
      Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textXs, "flex", "gap-3"]] $ do
        Lucid.span_ [] $ Lucid.toHtml ([i|Ep. #{epNum}|] :: Text)
        Lucid.span_ [] $ Lucid.toHtml $ formatDate sr.srScheduledAt
        case sr.srDurationSeconds of
          Just dur -> Lucid.span_ [] $ Lucid.toHtml $ formatDuration dur
          Nothing -> mempty
    Lucid.form_
      [ Lucid.method_ "post",
        Lucid.action_ [i|/#{forceEpisodeUrl}|],
        hxPost_ [i|/#{forceEpisodeUrl}|],
        hxSwap_ "none",
        hxConfirm_ "Force-play this episode? This will interrupt the current stream.",
        hxDisabledElt_ "find button"
      ]
      $ do
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "episodeId", Lucid.value_ [i|#{epId}|]]
        Lucid.button_
          [ Lucid.type_ "submit",
            class_ $ base [Tokens.px4, Tokens.py2, Tokens.fontBold, Tokens.textXs, Tokens.border2, Tokens.warningBorder, Tokens.warningText, Tokens.warningBg, "hover:opacity-80", "whitespace-nowrap"]
          ]
          "FORCE PLAY"
  where
    formatDate utc = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (utcToPacific utc)
    formatDuration :: Int64 -> Text
    formatDuration secs =
      let m = secs `div` 60
          s = secs `mod` 60
       in [i|#{m}:#{Text.justifyRight 2 '0' (Text.pack (show s))}|]
