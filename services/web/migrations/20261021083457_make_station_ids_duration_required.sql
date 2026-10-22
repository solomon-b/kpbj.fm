-- Make the length of every station ID required.
--
-- Uploads measure the length with ffprobe and refuse a file with no length.
-- The rows uploaded before that were filled in by hand with
-- scripts/station-id-durations-sql.sh. This migration fails if one is still
-- NULL, so run that script against the database first.

ALTER TABLE station_ids ALTER COLUMN duration_seconds SET NOT NULL;

COMMENT ON COLUMN station_ids.duration_seconds IS 'Audio length in whole seconds, rounded up, measured by ffprobe when the file was staged';
