{-# LANGUAGE QuasiQuotes #-}

-- | A small play and pause button for one audio file in a table row.
--
-- The button controls a hidden @<audio>@ element. Alpine.js keeps the
-- playing state, so the button shows the play icon or the pause icon.
module Component.InlineAudioPlayer
  ( renderInlineAudioPlayer,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine

--------------------------------------------------------------------------------

-- | Render an inline audio player with Alpine.js.
renderInlineAudioPlayer ::
  -- | Full URL of the audio file
  Text ->
  Lucid.Html ()
renderInlineAudioPlayer audioUrl =
  Lucid.div_
    [ xData_ [i|{ playing: false, audio: null }|],
      class_ $ base ["inline-flex", "items-center", Tokens.gap2]
    ]
    $ do
      -- Hidden audio element
      Lucid.audio_
        [ xRef_ "audioPlayer",
          Lucid.src_ audioUrl,
          Lucid.class_ "hidden"
        ]
        mempty
      -- Play/Pause button
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_
            [i|
            if (!$refs.audioPlayer) return;
            if (playing) {
              $refs.audioPlayer.pause();
              playing = false;
            } else {
              $refs.audioPlayer.play();
              playing = true;
              $refs.audioPlayer.onended = () => { playing = false; };
            }
          |],
          class_ $ base ["p-2", "rounded", Tokens.hoverBg, "transition-colors"],
          Lucid.title_ "Play/Pause"
        ]
        $ do
          -- Play icon (shown when not playing)
          Lucid.i_
            [ Lucid.class_ [i|fa-solid fa-play #{Tokens.fgMuted}|],
              Lucid.term "x-show" "!playing"
            ]
            mempty
          -- Pause icon (shown when playing)
          Lucid.i_
            [ Lucid.class_ [i|fa-solid fa-pause #{Tokens.fgMuted}|],
              Lucid.term "x-show" "playing"
            ]
            mempty
