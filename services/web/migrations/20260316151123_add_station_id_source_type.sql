-- Allow 'station_id' as a source_type in playback_history.
-- Needed for logging station ID plays from the fallback system.

ALTER TABLE playback_history DROP CONSTRAINT playback_history_source_type_check;
ALTER TABLE playback_history ADD CONSTRAINT playback_history_source_type_check
  CHECK (source_type IN ('episode', 'ephemeral', 'station_id'));
