-- A rerun is an earlier episode of a show. It airs in a slot that has no new
-- episode.
ALTER TABLE playback_history DROP CONSTRAINT playback_history_source_type_check;
ALTER TABLE playback_history ADD CONSTRAINT playback_history_source_type_check
  CHECK (source_type IN (
    'episode', 'rerun', 'ephemeral', 'station_id', 'psa', 'underwriting', 'announcement', 'show_promo'
  ));
