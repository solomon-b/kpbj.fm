{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Episode
  ( renderEpisodeCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, showEpisodesLinks)
import API.Types
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (d_, hxGet_, hxPushUrl_, hxTarget_, path_, svg_, viewBox_, xData_, xOnClick_, xShow_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------
-- URL Helpers

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

episodeDetailUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDetailUrl showSlug episodeNumber =
  Links.linkURI $ showEpisodesLinks.detail showSlug episodeNumber

--------------------------------------------------------------------------------
-- Main Render Function

-- | Render an episode card with artwork (with play button overlay) and date.
--
-- When canViewDrafts is True and the episode is a draft, a "DRAFT" badge is displayed.
renderEpisodeCard :: Shows.Model -> Bool -> Episodes.Model -> Lucid.Html ()
renderEpisodeCard showModel canViewDrafts episode = do
  let epUrl = episodeDetailUrl showModel.slug episode.episodeNumber
      showTitle = showModel.title
      episodeNum = episode.episodeNumber
      episodeId = episode.id
      mAudioPath = episode.audioFilePath
      mArtworkUrl = episode.artworkUrl
      playerId = [i|episode-#{episodeId}|] :: Text
      audioUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) mAudioPath
      hasAudio = if isJust mAudioPath then "true" else "false" :: Text
      episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|] :: Text
      isDraft = episode.status == Episodes.Draft

  -- Container with Alpine.js state for audio player
  -- Note: Audio playback is delegated to the persistent navbar player,
  -- so no local audio element is needed here.
  Lucid.div_
    [ class_ $ base [Tokens.bgWhite],
      xData_ $ audioPlayerScript playerId hasAudio audioUrl episodeMetadata
    ]
    $ do
      -- Artwork with play button overlay and optional draft badge
      renderArtworkWithPlayer epUrl mArtworkUrl (canViewDrafts && isDraft)

      -- Episode date
      renderEpisodeDate episode.scheduledAt

--------------------------------------------------------------------------------
-- Component Functions

-- | Render artwork with play button overlayed on bottom-left corner.
renderArtworkWithPlayer :: Links.URI -> Maybe Text -> Bool -> Lucid.Html ()
renderArtworkWithPlayer epUrl mArtworkUrl showDraftBadge =
  Lucid.div_ [class_ $ base ["relative", Tokens.mb4]] $ do
    -- Clickable artwork image
    Lucid.a_
      [ Lucid.href_ [i|/#{epUrl}|],
        hxGet_ [i|/#{epUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.fullWidth, "aspect-[4/3]", "bg-gray-300", "flex", "items-center", "justify-center", Tokens.textXs, "block"]
      ]
      $ case mArtworkUrl of
        Just artworkUrl ->
          Lucid.img_
            [ Lucid.src_ [i|/#{mediaGetUrl}/#{artworkUrl}|],
              Lucid.alt_ "Episode artwork",
              Lucid.class_ "w-full h-full object-cover"
            ]
        Nothing -> "[EP IMG]"

    -- Draft badge (top-right corner) - only shown for draft episodes when user can see drafts
    when showDraftBadge $
      Lucid.span_
        [ class_ $ base ["absolute", "top-2", "right-2", "bg-yellow-500", "text-black", Tokens.textXs, Tokens.fontBold, "px-2", "py-1"]
        ]
        "DRAFT"

    -- Play button overlay (bottom-left corner)
    renderPlayButton

-- | Render the play/pause button overlay.
renderPlayButton :: Lucid.Html ()
renderPlayButton =
  Lucid.button_
    [ xOnClick_ "toggle()",
      class_ $ base ["absolute", "bottom-2", "left-2", "w-12", "h-12", "bg-black/70", "hover:bg-black/90", Tokens.textWhite, "rounded-full", "flex", "items-center", "justify-center", "transition-colors"]
    ]
    $ do
      -- Play icon (shown when not playing)
      Lucid.span_ [xShow_ "!isPlaying", Lucid.class_ "pl-1"] $
        svg_ [Lucid.class_ "w-7 h-7 fill-current", viewBox_ "0 0 24 24"] $
          path_ [d_ "M8 5v14l11-7z"] mempty
      -- Pause icon (shown when playing)
      Lucid.span_ [xShow_ "isPlaying"] $
        svg_ [Lucid.class_ "w-7 h-7 fill-current", viewBox_ "0 0 24 24"] $
          path_ [d_ "M6 19h4V5H6v14zm8-14v14h4V5h-4z"] mempty

-- | Render episode date.
renderEpisodeDate :: Maybe UTCTime -> Lucid.Html ()
renderEpisodeDate mScheduledAt =
  Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] $
    case mScheduledAt of
      Just scheduledAt -> do
        let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" scheduledAt
        Lucid.toHtml dateStr
      Nothing -> mempty

--------------------------------------------------------------------------------
-- Alpine.js Script

-- | Generate Alpine.js data object for the audio player.
--
-- Instead of playing audio locally, delegates playback to the persistent
-- navbar player. This allows audio to continue playing across page navigation.
audioPlayerScript :: Text -> Text -> Text -> Text -> Text
audioPlayerScript playerId hasAudio audioUrl episodeMetadata =
  [i|{
  playerId: '#{playerId}',
  hasAudio: #{hasAudio},
  audioUrl: '#{audioUrl}',
  title: '#{episodeMetadata}',

  // Check if navbar player is currently playing this episode
  get isPlaying() {
    return isNavbarPlayingEpisode(this.audioUrl);
  },

  toggle() {
    if (!this.hasAudio) return;
    // Delegate playback to the navbar player
    toggleEpisodeInNavbar(this.audioUrl, this.title);
  },

  // Keep pause() method for global coordination (pauseOtherPlayers may call this)
  pause() {
    // No-op since we're not playing locally, but update local state
    // The navbar player handles actual pausing
  }
}|]
