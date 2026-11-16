-- Require a length on every episode that has audio.
--
-- The playout timeline treats an episode with no length as unscheduled time,
-- so that episode does not air. Uploads measure the length with ffprobe, and
-- the episode forms refuse audio with no length. The rows uploaded before that
-- were filled in with scripts/audio-durations-sql.sh. This migration fails if
-- one is still NULL, so run that script against the database first.
--
-- An episode with no audio keeps a NULL length. Clearing the audio keeps the
-- old length in the row, which this CHECK allows.

ALTER TABLE episodes
  ADD CONSTRAINT episodes_audio_has_duration CHECK (
    audio_file_path IS NULL OR duration_seconds IS NOT NULL
  );
