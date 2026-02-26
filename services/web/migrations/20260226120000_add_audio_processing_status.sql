-- Add audio processing status tracking to episodes.
--
-- Supports moving ffmpeg loudness normalization from synchronous upload
-- to a nightly batch job. New episodes get status 'unprocessed' on upload;
-- the batch job transitions through 'processing' → 'processed' or 'failed'.
-- NULL means legacy episode (no processing needed).

ALTER TABLE episodes
  ADD COLUMN audio_processing_status TEXT DEFAULT NULL,
  ADD COLUMN audio_processing_attempts INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN original_audio_file_path TEXT DEFAULT NULL;

-- Index for batch job query (only scans unprocessed/failed rows)
CREATE INDEX idx_episodes_audio_processing_status
  ON episodes (audio_processing_status)
  WHERE audio_processing_status IN ('unprocessed', 'failed');
