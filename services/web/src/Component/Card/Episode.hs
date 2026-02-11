{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Episode
  ( renderEpisodeCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (showEpisodesLinks)
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
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Alpine
import Lucid.HTML5.Extra
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (escapeJsString)

--------------------------------------------------------------------------------
-- URL Helpers

episodeDetailUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDetailUrl showSlug episodeNumber =
  Links.linkURI $ showEpisodesLinks.detail showSlug episodeNumber

--------------------------------------------------------------------------------
-- Main Render Function

-- | Render an episode card with artwork (with play button overlay) and date.
renderEpisodeCard :: StorageBackend -> Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeCard backend showModel episode = do
  let epUrl = episodeDetailUrl showModel.slug episode.episodeNumber
      showTitle = showModel.title
      episodeNum = episode.episodeNumber
      episodeId = episode.id
      mAudioPath = episode.audioFilePath
      mArtworkUrl = episode.artworkUrl
      playerId = [i|episode-#{episodeId}|] :: Text
      audioUrl = maybe "" (buildMediaUrl backend) mAudioPath
      hasAudio = if isJust mAudioPath then "true" else "false" :: Text
      episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|] :: Text

  -- Container with Alpine.js state for audio player
  -- Note: Audio playback is delegated to the persistent navbar player,
  -- so no local audio element is needed here.
  Lucid.div_
    [ class_ $ base [Tokens.bgMain],
      xData_ $ audioPlayerScript playerId hasAudio audioUrl episodeMetadata
    ]
    $ do
      -- Artwork with play button overlay
      renderArtworkWithPlayer backend epUrl mArtworkUrl

      -- Episode date
      renderEpisodeDate episode.scheduledAt

--------------------------------------------------------------------------------
-- Component Functions

-- | Render artwork with play button overlayed on bottom-left corner.
renderArtworkWithPlayer :: StorageBackend -> Links.URI -> Maybe Text -> Lucid.Html ()
renderArtworkWithPlayer backend epUrl mArtworkUrl =
  Lucid.div_ [class_ $ base ["relative", Tokens.mb4]] $ do
    -- Clickable artwork image
    Lucid.a_
      [ Lucid.href_ [i|/#{epUrl}|],
        hxGet_ [i|/#{epUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.fullWidth, "aspect-[4/3]", Tokens.bgAlt, "flex", "items-center", "justify-center", Tokens.textXs, "block", "border", Tokens.borderMuted]
      ]
      $ case mArtworkUrl of
        Just artworkPath ->
          Lucid.img_
            [ Lucid.src_ (buildMediaUrl backend artworkPath),
              Lucid.alt_ "Episode artwork",
              Lucid.class_ "w-full h-full object-cover"
            ]
        Nothing -> "[EP IMG]"

    -- Play button overlay (bottom-left corner)
    renderPlayButton

    -- Scrub bar overlay (bottom edge of artwork)
    renderScrubBar

-- | Render the play/pause button overlay.
renderPlayButton :: Lucid.Html ()
renderPlayButton =
  Lucid.button_
    [ xOnClick_ "toggle()",
      class_ $ base ["absolute", "bottom-2", "left-2", "w-12", "h-12", "bg-black/70", "hover:bg-black/90", "text-white/80", "rounded-full", "flex", "items-center", "justify-center", "transition-colors"]
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

-- | Render the scrub/progress bar at the bottom of episode artwork.
renderScrubBar :: Lucid.Html ()
renderScrubBar =
  Lucid.div_
    [ xShow_ "hasAudio",
      xRef_ "scrubBar",
      xOnMousedown_ "scrubStart($event)",
      xOnTouchstart_ "scrubStart($event)",
      class_ $ base ["absolute", "bottom-0", "left-0", "right-0", "z-10", "h-2", "hover:h-3", "transition-all", "cursor-pointer", "bg-black/30", "group"]
    ]
    $ do
      -- Progress fill
      Lucid.div_
        [ xBindStyle_ "'width: ' + progress + '%'",
          class_ $ base ["h-full", "bg-white/80", "group-hover:bg-white", "pointer-events-none"]
        ]
        mempty

-- | Render episode date.
renderEpisodeDate :: UTCTime -> Lucid.Html ()
renderEpisodeDate scheduledAt =
  Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $ do
    let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" scheduledAt
    Lucid.toHtml dateStr

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
  audioUrl: '#{escapeJsString audioUrl}',
  title: '#{escapeJsString episodeMetadata}',

  // Scrub bar state
  currentTime: 0,
  duration: 0,
  scrubbing: false,
  scrubPosition: 0,
  _rafId: null,

  // Check if navbar player is currently playing this episode
  get isPlaying() {
    return isNavbarPlayingEpisode(this.audioUrl);
  },

  get progress() {
    if (this.scrubbing) return this.scrubPosition;
    if (this.duration <= 0) return 0;
    return (this.currentTime / this.duration) * 100;
  },

  init() {
    this.$watch('isPlaying', (playing) => {
      if (playing) {
        this._startTimeSync();
      } else {
        this._stopTimeSync();
      }
    });
    // Sync immediately if already playing (e.g. after HTMX navigation)
    if (this.isPlaying) {
      this._startTimeSync();
    }
  },

  destroy() {
    this._stopTimeSync();
  },

  _startTimeSync() {
    if (this._rafId) return;
    const sync = () => {
      const state = getNavbarEpisodeState(this.audioUrl);
      this.currentTime = state.currentTime;
      this.duration = state.duration;
      this._rafId = requestAnimationFrame(sync);
    };
    this._rafId = requestAnimationFrame(sync);
  },

  _stopTimeSync() {
    if (this._rafId) {
      cancelAnimationFrame(this._rafId);
      this._rafId = null;
    }
  },

  seekTo(event) {
    const bar = this.$refs.scrubBar;
    if (!bar) return;
    const rect = bar.getBoundingClientRect();
    const pct = Math.max(0, Math.min(1, (event.clientX - rect.left) / rect.width));

    if (!this.isPlaying) {
      // Start playback then seek after audio loads
      toggleEpisodeInNavbar(this.audioUrl, this.title);
      setTimeout(() => {
        const state = getNavbarEpisodeState(this.audioUrl);
        if (state.duration > 0) {
          seekNavbarEpisode(this.audioUrl, pct * state.duration);
        }
      }, 200);
    } else {
      seekNavbarEpisode(this.audioUrl, pct * this.duration);
    }
  },

  scrubStart(event) {
    this.scrubbing = true;
    const bar = this.$refs.scrubBar;
    if (!bar) return;
    const rect = bar.getBoundingClientRect();
    const isTouch = event.type === 'touchstart';
    const clientX = isTouch ? event.touches[0].clientX : event.clientX;
    this.scrubPosition = Math.max(0, Math.min(100, ((clientX - rect.left) / rect.width) * 100));

    const onMove = (e) => {
      const x = e.type.startsWith('touch') ? e.touches[0].clientX : e.clientX;
      const r = bar.getBoundingClientRect();
      this.scrubPosition = Math.max(0, Math.min(100, ((x - r.left) / r.width) * 100));
    };

    const onEnd = (e) => {
      const x = e.type.startsWith('touch') ? e.changedTouches[0].clientX : e.clientX;
      const r = bar.getBoundingClientRect();
      const pct = Math.max(0, Math.min(1, (x - r.left) / r.width));
      this.scrubbing = false;

      if (!this.isPlaying) {
        toggleEpisodeInNavbar(this.audioUrl, this.title);
        setTimeout(() => {
          const state = getNavbarEpisodeState(this.audioUrl);
          if (state.duration > 0) {
            seekNavbarEpisode(this.audioUrl, pct * state.duration);
          }
        }, 200);
      } else {
        seekNavbarEpisode(this.audioUrl, pct * this.duration);
      }

      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup', onEnd);
      document.removeEventListener('touchmove', onMove);
      document.removeEventListener('touchend', onEnd);
    };

    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup', onEnd);
    document.addEventListener('touchmove', onMove);
    document.addEventListener('touchend', onEnd);
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
