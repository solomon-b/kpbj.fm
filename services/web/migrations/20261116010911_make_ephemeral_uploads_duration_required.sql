-- Make the length of every ephemeral upload required.
--
-- Uploads measure the length with ffprobe and refuse a file with no length.
-- The rows uploaded before that were filled in with
-- scripts/audio-durations-sql.sh after the v0.13.3 deploy. This migration
-- fails if one is still NULL, so run that script against the database first.

ALTER TABLE ephemeral_uploads ALTER COLUMN duration_seconds SET NOT NULL;

COMMENT ON COLUMN ephemeral_uploads.duration_seconds IS 'Audio length in whole seconds, rounded up, measured by ffprobe when the file was staged';
