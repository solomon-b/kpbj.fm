{-# LANGUAGE QuasiQuotes #-}

module Component.AudioPlayer.Waveform
  ( Config (..),
    render,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras (xBindStyle_, xData_, xIf_, xOnClick_, xRef_, xText_)

--------------------------------------------------------------------------------

-- | Configuration for the waveform audio player component.
data Config = Config
  { -- | Unique identifier for this player instance (used for pauseOtherPlayers coordination)
    playerId :: Text,
    -- | Full URL to the audio file
    audioUrl :: Text,
    -- | Display title (e.g., "Show Name - Episode 42")
    title :: Text
  }

--------------------------------------------------------------------------------

-- | Render a waveform audio player with play/pause, seek, and time display.
--
-- The player generates a waveform visualization from the audio file using the
-- Web Audio API and displays it on a canvas. Users can click on the waveform
-- to seek to different positions.
render :: Config -> Lucid.Html ()
render Config {..} = do
  Lucid.div_
    [ class_ $ base [Tokens.bgGray100, "border-2", "border-gray-600", Tokens.p6],
      xData_ (alpineScript playerId audioUrl title)
    ]
    $ do
      Lucid.audio_ [xRef_ "audio", Lucid.preload_ "metadata"] mempty

      -- Play/pause button and waveform
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.mb4]] $ do
        Lucid.button_
          [ class_ $ base [Tokens.bgGray800, Tokens.textWhite, "px-8", "py-3", Tokens.fontBold, "hover:bg-gray-700", Tokens.textLg],
            xOnClick_ "toggle()",
            xText_ "isPlaying ? 'PAUSE' : 'PLAY'"
          ]
          "PLAY"

        -- Waveform container
        Lucid.div_ [Lucid.class_ "flex-grow relative"] $ do
          -- Loading state
          Lucid.template_ [xIf_ "waveformLoading"] $ do
            Lucid.div_ [class_ $ base ["bg-gray-300", "h-16", "rounded", "flex", "items-center", "justify-center"]] $ do
              Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] "Loading waveform..."

          -- Error state - fallback to simple progress bar
          Lucid.template_ [xIf_ "waveformError"] $ do
            Lucid.div_ [class_ $ base ["bg-gray-300", "h-16", "rounded", "relative", "cursor-pointer"], xOnClick_ "seek($event)"] $ do
              Lucid.div_
                [ class_ $ base [Tokens.bgGray800, "h-16", "rounded", "absolute", "top-0", "left-0"],
                  xBindStyle_ "{ width: progress + '%' }"
                ]
                mempty

          -- Waveform canvas
          Lucid.template_ [xIf_ "!waveformLoading && !waveformError"] $ do
            Lucid.canvas_
              [ xRef_ "waveformCanvas",
                class_ $ base [Tokens.fullWidth, "h-16", "cursor-pointer", "rounded"],
                xOnClick_ "seek($event)"
              ]
              mempty

        Lucid.span_
          [ class_ $ base [Tokens.textSm, "font-mono", "w-28", "text-right"],
            xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"
          ]
          "0:00 / 0:00"

--------------------------------------------------------------------------------
-- Alpine.js Script

alpineScript :: Text -> Text -> Text -> Text
alpineScript playerId' audioUrl' title' =
  [i|{
  playerId: '#{playerId'}',
  isPlaying: false,
  audioUrl: '#{audioUrl'}',
  title: '#{title'}',
  currentTime: 0,
  duration: 0,
  waveformData: [],
  waveformLoading: true,
  waveformError: false,
  init() {
    const audio = this.$refs.audio;
    audio.addEventListener('loadedmetadata', () => {
      this.duration = audio.duration;
    });
    audio.addEventListener('timeupdate', () => {
      this.currentTime = audio.currentTime;
      this.drawWaveform();
    });
    audio.addEventListener('ended', () => {
      this.isPlaying = false;
    });
    this.loadWaveform();
  },
  async loadWaveform() {
    try {
      const response = await fetch(this.audioUrl);
      const arrayBuffer = await response.arrayBuffer();
      const audioContext = new (window.AudioContext || window.webkitAudioContext)();
      const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);

      const rawData = audioBuffer.getChannelData(0);
      const samples = 500;
      const blockSize = Math.floor(rawData.length / samples);
      const peaks = [];

      for (let i = 0; i < samples; i++) {
        let sum = 0;
        for (let j = 0; j < blockSize; j++) {
          sum += Math.abs(rawData[(i * blockSize) + j]);
        }
        peaks.push(sum / blockSize);
      }

      const maxPeak = Math.max(...peaks);
      this.waveformData = peaks.map(p => p / maxPeak);
      this.waveformLoading = false;
      this.$nextTick(() => this.drawWaveform());
      audioContext.close();
    } catch (e) {
      console.error('Failed to load waveform:', e);
      this.waveformError = true;
      this.waveformLoading = false;
    }
  },
  drawWaveform() {
    const canvas = this.$refs.waveformCanvas;
    if (!canvas || this.waveformData.length === 0) return;

    const ctx = canvas.getContext('2d');
    const dpr = window.devicePixelRatio || 1;
    const rect = canvas.getBoundingClientRect();

    canvas.width = rect.width * dpr;
    canvas.height = rect.height * dpr;
    ctx.scale(dpr, dpr);

    const width = rect.width;
    const height = rect.height;
    const barWidth = width / this.waveformData.length;
    const progressPercent = this.duration ? this.currentTime / this.duration : 0;
    const progressX = width * progressPercent;

    ctx.clearRect(0, 0, width, height);

    this.waveformData.forEach((peak, i) => {
      const x = i * barWidth;
      const barHeight = Math.max(2, peak * (height * 0.9));
      const y = (height - barHeight) / 2;

      ctx.fillStyle = x < progressX ? 'rgb(31, 41, 55)' : 'rgb(156, 163, 175)';
      ctx.fillRect(x, y, Math.max(1, barWidth - 0.5), barHeight);
    });
  },
  toggle() {
    this.isPlaying ? this.pause() : this.play();
  },
  play() {
    if (typeof pauseOtherPlayers !== 'undefined') {
      pauseOtherPlayers(this.playerId);
    }
    const audio = this.$refs.audio;
    if (!audio.src) audio.src = this.audioUrl;
    audio.play().then(() => { this.isPlaying = true; });
  },
  pause() {
    const audio = this.$refs.audio;
    audio.pause();
    this.isPlaying = false;
  },
  seek(event) {
    const canvas = this.$refs.waveformCanvas;
    const rect = canvas.getBoundingClientRect();
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
