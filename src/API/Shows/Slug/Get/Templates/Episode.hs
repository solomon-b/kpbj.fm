{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Get.Templates.Episode
  ( renderEpisode,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, showEpisodesLinks)
import API.Types
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
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xOnClick_, xRef_, xShow_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------
-- URL Helpers

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

episodeDetailUrl :: Slug -> Episodes.Id -> Slug -> Links.URI
episodeDetailUrl showSlug episodeId episodeSlug =
  Links.linkURI $ showEpisodesLinks.detailWithSlug showSlug episodeId episodeSlug

--------------------------------------------------------------------------------
-- Main Render Function

-- | Render an episode card with artwork (with play button overlay) and date.
renderEpisode :: Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisode showModel episode = do
  let epUrl = episodeDetailUrl showModel.slug episode.id episode.slug
      showTitle = showModel.title
      episodeTitle = episode.title
      episodeNum = episode.episodeNumber
      episodeId = episode.id
      mAudioPath = episode.audioFilePath
      mArtworkUrl = episode.artworkUrl
      playerId = [i|episode-#{episodeId}|] :: Text
      audioUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) mAudioPath
      hasAudio = if isJust mAudioPath then "true" else "false" :: Text
      episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}: #{episodeTitle}|] :: Text

  -- Container with Alpine.js state for audio player
  Lucid.div_
    [ class_ $ base [Tokens.bgWhite],
      xData_ $ audioPlayerScript playerId hasAudio audioUrl episodeMetadata
    ]
    $ do
      -- Hidden audio element
      Lucid.audio_ [xRef_ "audio", Lucid.preload_ "none"] mempty

      -- Artwork with play button overlay
      renderArtworkWithPlayer epUrl mArtworkUrl

      -- Episode date
      renderEpisodeDate episode.scheduledAt

--------------------------------------------------------------------------------
-- Component Functions

-- | Render artwork with play button overlayed on bottom-left corner.
renderArtworkWithPlayer :: Links.URI -> Maybe Text -> Lucid.Html ()
renderArtworkWithPlayer epUrl mArtworkUrl =
  Lucid.div_ [class_ $ base ["relative", Tokens.mb4]] $ do
    -- Clickable artwork image
    Lucid.a_
      [ Lucid.href_ [i|/#{epUrl}|],
        hxGet_ [i|/#{epUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.fullWidth, "aspect-[4/3]", "bg-gray-300", "border", "border-gray-600", "flex", "items-center", "justify-center", Tokens.textXs, "block"]
      ]
      $ case mArtworkUrl of
        Just artworkUrl ->
          Lucid.img_
            [ Lucid.src_ [i|/#{mediaGetUrl}/#{artworkUrl}|],
              Lucid.alt_ "Episode artwork",
              Lucid.class_ "w-full h-full object-cover"
            ]
        Nothing -> "[EP IMG]"

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
      Lucid.span_ [xShow_ "!isPlaying", Lucid.class_ "text-xl pl-1"] "▶"
      -- Pause icon (shown when playing)
      Lucid.span_ [xShow_ "isPlaying", Lucid.class_ "text-xl"] "⏸"

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
audioPlayerScript :: Text -> Text -> Text -> Text -> Text
audioPlayerScript playerId hasAudio audioUrl episodeMetadata =
  [i|{
  playerId: '#{playerId}',
  isPlaying: false,
  hasAudio: #{hasAudio},
  audioUrl: '#{audioUrl}',
  title: '#{episodeMetadata}',

  toggle() {
    if (!this.hasAudio) return;
    this.isPlaying ? this.pause() : this.play();
  },

  play() {
    if (!this.hasAudio) return;
    pauseOtherPlayers(this.playerId);
    const audio = this.$refs.audio;
    if (!audio.src) audio.src = this.audioUrl;
    audio.play().then(() => { this.isPlaying = true; });
  },

  pause() {
    const audio = this.$refs.audio;
    audio.pause();
    this.isPlaying = false;
  }
}|]
