-- Record the audio length on each staged upload.
--
-- The upload handler measures the file with ffprobe before it stages the
-- file. The save handlers read the length from this row when they claim the
-- upload. The browser no longer carries the length back in a form field.

ALTER TABLE staged_uploads ADD COLUMN duration_seconds BIGINT;

COMMENT ON COLUMN staged_uploads.duration_seconds IS 'Audio length in seconds, measured by ffprobe at upload. NULL for an upload that is not audio';
