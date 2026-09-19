-- Allow 'psa' and 'advertisement' as source_type values in playback_history.
-- Needed for logging break window plays.
--
-- The two categories are recorded separately rather than under one 'break_item'
-- value. An airplay report has to answer which advertisements aired, and
-- playback_history carries no break_item_id, so a single value would leave the
-- category recoverable only by matching a title or a URL back to break_items.

ALTER TABLE playback_history DROP CONSTRAINT playback_history_source_type_check;
ALTER TABLE playback_history ADD CONSTRAINT playback_history_source_type_check
  CHECK (source_type IN ('episode', 'ephemeral', 'station_id', 'psa', 'advertisement'));
