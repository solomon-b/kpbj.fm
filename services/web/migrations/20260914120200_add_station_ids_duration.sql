-- Record how long a station ID runs.
--
-- A break window is two minutes and always opens with one station ID. The
-- playout handler subtracts this from the budget before it picks break items.
-- Rows that predate this column read as a default in that calculation.

ALTER TABLE station_ids ADD COLUMN duration_seconds BIGINT;

COMMENT ON COLUMN station_ids.duration_seconds IS 'Audio length, read in the browser at upload time. NULL on rows uploaded before the break window existed';
