-- Record how long a station ID runs.
--
-- A break is two minutes and always opens with one station ID. The planner
-- subtracts this from the break's budget before it places anything else. Rows
-- that predate this column read as a default in that calculation.

ALTER TABLE station_ids ADD COLUMN duration_seconds BIGINT;

COMMENT ON COLUMN station_ids.duration_seconds IS 'Audio length, measured by ffprobe when the file was staged. NULL on rows uploaded before the break window existed';
