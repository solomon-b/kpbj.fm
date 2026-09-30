-- Record break plays in playback_history.
--
-- PSAs and underwriting announcements use their own source_type values, so a
-- report can tell them apart. break_item_id links each break play to its row,
-- so delivery counts and the underwriting report do not match titles or URLs.

ALTER TABLE playback_history DROP CONSTRAINT playback_history_source_type_check;
ALTER TABLE playback_history ADD CONSTRAINT playback_history_source_type_check
  CHECK (source_type IN ('episode', 'ephemeral', 'station_id', 'psa', 'underwriting'));

ALTER TABLE playback_history ADD COLUMN break_item_id BIGINT REFERENCES break_items(id);

CREATE INDEX idx_playback_history_break_item_started
    ON playback_history (break_item_id, started_at)
    WHERE break_item_id IS NOT NULL;
