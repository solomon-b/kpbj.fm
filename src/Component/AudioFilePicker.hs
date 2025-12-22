{-# LANGUAGE QuasiQuotes #-}

module Component.AudioFilePicker
  ( Config (..),
    render,
  )
where

--------------------------------------------------------------------------------

import Component.AudioPlayer.Waveform qualified as WaveformPlayer
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Configuration for the audio file picker component.
data Config = Config
  { -- | Unique identifier for the player instance
    playerId :: Text,
    -- | Display title for the audio (e.g., "Show Name - Episode 42")
    title :: Text,
    -- | URL to existing audio file (empty string if none)
    existingAudioUrl :: Text,
    -- | Whether this field is required
    isRequired :: Bool
  }

--------------------------------------------------------------------------------

-- | Render an audio file picker with integrated player.
--
-- Combines a file input and audio player in one dashed border box.
-- The player shows the existing file (if any) or updates when a new file is selected.
render :: Config -> Lucid.Html ()
render Config {..} = do
  let label :: Text
      label = if isRequired then "Audio File *" else "Audio File"

  Lucid.div_ $ do
    -- Label
    Lucid.label_
      [Lucid.for_ "audio_file-input", class_ $ base [Tokens.fontBold, Tokens.textSm, "uppercase", "tracking-wide", Tokens.mb2, "block"]]
      (Lucid.toHtml label)

    -- Dashed border container with player and file picker
    Lucid.div_ [class_ $ base ["border-2", "border-dashed", "border-gray-400", Tokens.p6, "text-center"]] $ do
      -- Audio player
      Lucid.div_ [class_ $ base [Tokens.mb4]] $
        WaveformPlayer.render
          WaveformPlayer.Config
            { WaveformPlayer.playerId = playerId,
              WaveformPlayer.audioUrl = existingAudioUrl,
              WaveformPlayer.title = title,
              WaveformPlayer.fileInputName = Just "audio_file"
            }

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ "audio_file",
          Lucid.id_ "audio_file-input",
          Lucid.accept_ "audio/*",
          Lucid.class_ "hidden"
        ]
          <> [Lucid.required_ "" | isRequired]

      -- File picker button and info
      Lucid.label_ [Lucid.for_ "audio_file-input", Lucid.class_ "cursor-pointer"] $ do
        Lucid.div_ [class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700", "inline-block"]] $
          "CHOOSE AUDIO FILE"
        Lucid.div_ [class_ $ base ["mt-2", Tokens.textSm, Tokens.textGray600]] $
          if isRequired
            then "MP3, WAV, FLAC accepted • Max 500MB"
            else "MP3, WAV, FLAC accepted • Max 500MB • Leave empty to keep current file"
