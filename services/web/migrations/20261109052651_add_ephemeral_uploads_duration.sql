-- Record how long an ephemeral upload runs.
--
-- The playout timeline places each ephemeral track at a fixed time, so it
-- needs the length. New uploads copy it from the staged upload. Rows uploaded
-- before this column get it from scripts/audio-durations-sql.sh.

ALTER TABLE ephemeral_uploads ADD COLUMN duration_seconds BIGINT;

COMMENT ON COLUMN ephemeral_uploads.duration_seconds IS 'Audio length in whole seconds, rounded up, measured by ffprobe when the file was staged. NULL on rows uploaded before this column existed';
