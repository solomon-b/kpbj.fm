{-# LANGUAGE QuasiQuotes #-}

module Component.AudioPlayer.Waveform
  ( Config (..),
    render,

    -- * Default Styles
    defaultContainerClasses,
    defaultButtonClasses,
    defaultProgressBarClasses,
    defaultProgressFillClasses,
    defaultTimeDisplayClasses,
  )
where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_, class_')
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine
import Utils (escapeJsString)

--------------------------------------------------------------------------------

-- | Configuration for the audio player component.
--
-- NOTE: Waveform visualization is not yet implemented. Radio episodes are
-- typically 1-2 hours long, and client-side waveform generation is too slow.
-- See docs/feature-notes/staged-uploads-and-waveforms.md for the planned
-- server-side waveform generation approach.
data Config = Config
  { -- | Unique identifier for this player instance (used for pauseOtherPlayers coordination)
    playerId :: Text,
    -- | Full URL to the audio file (used as initial/fallback source)
    audioUrl :: Text,
    -- | Display title (e.g., "Show Name - Episode 42")
    title :: Text,
    -- | Optional file input ID to watch for new file selections.
    -- When a file is selected, the player will switch to preview that file instead.
    fileInputId :: Maybe Text,
    -- | Optional CSS classes for the outer container (default: gray background with border)
    containerClasses :: Maybe Text,
    -- | Optional CSS classes for the play/pause button
    buttonClasses :: Maybe Text,
    -- | Optional CSS classes for the progress bar container
    progressBarClasses :: Maybe Text,
    -- | Optional CSS classes for the progress bar fill
    progressFillClasses :: Maybe Text,
    -- | Optional CSS classes for the time display
    timeDisplayClasses :: Maybe Text
  }

--------------------------------------------------------------------------------

-- | Default CSS classes for each element.
defaultContainerClasses, defaultButtonClasses, defaultProgressBarClasses, defaultProgressFillClasses, defaultTimeDisplayClasses :: Text
defaultContainerClasses = class_' $ base [Tokens.bgAlt, "border-2", Tokens.borderDefault, Tokens.p6]
defaultButtonClasses = class_' $ base [Tokens.bgInverse, Tokens.fgInverse, "px-8", "py-3", Tokens.fontBold, "hover:opacity-80", Tokens.textLg]
defaultProgressBarClasses = class_' $ base ["flex-grow", Tokens.bgAlt, "h-8", "relative", "cursor-pointer", "rounded", "border", Tokens.borderMuted]
defaultProgressFillClasses = class_' $ base [Tokens.bgInverse, "h-8", "rounded", "absolute", "top-0", "left-0", "transition-all", "duration-100"]
defaultTimeDisplayClasses = class_' $ base [Tokens.textSm, "font-mono", "w-28", "text-right"]

-- | Render an audio player with play/pause, seek, and time display.
--
-- If 'fileInputId' is provided, the player will watch that file input and
-- automatically switch to previewing newly selected files.
render :: Config -> Lucid.Html ()
render Config {..} = do
  let fileInputSelector = maybe "" ("#" <>) fileInputId
      containerCls = fromMaybe defaultContainerClasses containerClasses
      buttonCls = fromMaybe defaultButtonClasses buttonClasses
      progressBarCls = fromMaybe defaultProgressBarClasses progressBarClasses
      progressFillCls = fromMaybe defaultProgressFillClasses progressFillClasses
      timeDisplayCls = fromMaybe defaultTimeDisplayClasses timeDisplayClasses

  Lucid.div_
    [ Lucid.class_ containerCls,
      xData_ (alpineScript playerId audioUrl title fileInputSelector)
    ]
    $ do
      Lucid.audio_ [xRef_ "audio", Lucid.preload_ "metadata"] mempty

      -- Play/pause button and progress bar
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.class_ buttonCls,
            xOnClick_ "toggle()",
            xText_ "isPlaying ? 'PAUSE' : 'PLAY'"
          ]
          "PLAY"

        -- Progress bar container
        Lucid.div_
          [ Lucid.class_ progressBarCls,
            xOnClick_ "seek($event)"
          ]
          $ do
            -- Progress fill
            Lucid.div_
              [ Lucid.class_ progressFillCls,
                xBindStyle_ "{ width: progress + '%' }"
              ]
              mempty

        -- Time display
        Lucid.span_
          [ Lucid.class_ timeDisplayCls,
            xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"
          ]
          "0:00 / 0:00"

--------------------------------------------------------------------------------
-- Alpine.js Script

alpineScript :: Text -> Text -> Text -> Text -> Text
alpineScript playerId' audioUrl' title' fileInputSelector =
  [i|{
  playerId: '#{escapeJsString playerId'}',
  isPlaying: false,
  originalUrl: '#{escapeJsString audioUrl'}',
  audioUrl: '#{escapeJsString audioUrl'}',
  title: '#{escapeJsString title'}',
  currentTime: 0,
  duration: 0,
  blobUrl: null,
  fileInputSelector: '#{escapeJsString fileInputSelector}',
  init() {
    const audio = this.$refs.audio;
    audio.addEventListener('loadedmetadata', () => {
      this.duration = audio.duration;
    });
    audio.addEventListener('timeupdate', () => {
      this.currentTime = audio.currentTime;
    });
    audio.addEventListener('ended', () => {
      this.isPlaying = false;
    });

    // Watch file input for changes if selector provided
    if (this.fileInputSelector) {
      const fileInput = document.querySelector(this.fileInputSelector);
      if (fileInput) {
        fileInput.addEventListener('change', (e) => this.handleFileSelect(e));
      }
    }

    // Listen for clear events
    window.addEventListener('waveform-player-clear', (e) => {
      if (e.detail && e.detail.playerId === this.playerId) {
        this.clearAudio();
      }
    });

    // Load audio metadata if we have a URL
    if (this.audioUrl) {
      audio.src = this.audioUrl;
    }
  },
  clearAudio() {
    this.pause();
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
      this.blobUrl = null;
    }
    this.audioUrl = '';
    this.originalUrl = '';
    const audio = this.$refs.audio;
    audio.removeAttribute('src');
    audio.load();
    this.currentTime = 0;
    this.duration = 0;
  },
  handleFileSelect(event) {
    const file = event.target.files[0];

    // Clean up old blob URL
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
      this.blobUrl = null;
    }

    // If no file selected (cleared), reset to original URL
    if (!file) {
      this.pause();
      this.audioUrl = this.originalUrl;
      const audio = this.$refs.audio;
      if (this.originalUrl) {
        audio.src = this.originalUrl;
      } else {
        audio.removeAttribute('src');
      }
      this.currentTime = 0;
      this.duration = 0;
      return;
    }

    // Create new blob URL for the selected file
    this.blobUrl = URL.createObjectURL(file);
    this.audioUrl = this.blobUrl;

    // Reset player state
    this.pause();
    const audio = this.$refs.audio;
    audio.src = this.audioUrl;
    this.currentTime = 0;
    this.duration = 0;
  },
  toggle() {
    this.isPlaying ? this.pause() : this.play();
  },
  play() {
    if (typeof pauseOtherPlayers !== 'undefined') {
      pauseOtherPlayers(this.playerId);
    }
    const audio = this.$refs.audio;
    audio.play().then(() => { this.isPlaying = true; });
  },
  pause() {
    const audio = this.$refs.audio;
    audio.pause();
    this.isPlaying = false;
  },
  seek(event) {
    const bar = event.currentTarget;
    const rect = bar.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const percent = x / rect.width;
    const audio = this.$refs.audio;
    if (this.duration) {
      audio.currentTime = percent * this.duration;
    }
  },
  formatTime(seconds) {
    if (!seconds || isNaN(seconds)) return '0:00';
    const hours = Math.floor(seconds / 3600);
    const mins = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);
    if (hours > 0) {
      return hours + ':' + (mins < 10 ? '0' : '') + mins + ':' + (secs < 10 ? '0' : '') + secs;
    }
    return mins + ':' + (secs < 10 ? '0' : '') + secs;
  },
  get progress() {
    if (!this.duration) return 0;
    return (this.currentTime / this.duration) * 100;
  }
}|]
