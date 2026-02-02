{-# LANGUAGE QuasiQuotes #-}

-- | Script component for extracting audio duration from uploaded files.
--
-- This script listens for file uploads on a staged audio field and
-- extracts the duration metadata, storing it in a hidden form field.
module Component.AudioDurationScript
  ( renderAudioDurationScript,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a script that extracts audio duration when a file is uploaded.
--
-- The script listens for changes on the file input with the given ID
-- and populates a hidden field named @duration_seconds@ with the
-- duration in seconds (rounded to nearest integer).
--
-- @
-- -- In your form template:
-- hidden "duration_seconds" ""
-- renderAudioDurationScript "audio_file-input"
-- @
--
-- The staged audio field from FormBuilder renders inputs with ID @{name}-input@,
-- so for a field named @audio_file@, pass @"audio_file-input"@.
renderAudioDurationScript ::
  -- | The ID of the file input element to listen on
  Text ->
  Lucid.Html ()
renderAudioDurationScript inputId =
  Lucid.script_
    [i|
(function() {
  const extractAudioDuration = (file) => {
    const isAudio = file.type.startsWith('audio/') || /\\.(mp3|wav|flac|aac|ogg|m4a)$/i.test(file.name);
    if (!isAudio) return;

    const audio = new Audio();
    audio.preload = 'metadata';

    audio.onloadedmetadata = () => {
      const durationField = document.querySelector('input[name="duration_seconds"]');
      if (durationField) {
        durationField.value = Math.round(audio.duration);
      }
      URL.revokeObjectURL(audio.src);
    };

    audio.onerror = () => URL.revokeObjectURL(audio.src);
    audio.src = URL.createObjectURL(file);
  };

  const audioInput = document.getElementById('#{inputId}');
  if (audioInput) {
    audioInput.addEventListener('change', (e) => {
      const file = e.target.files?.[0];
      if (file) extractAudioDuration(file);
    });
  }
})();
|]
