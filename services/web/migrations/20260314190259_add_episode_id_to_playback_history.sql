-- Add episode_id foreign key to playback_history.
-- Nullable because ephemeral uploads have no episode.

ALTER TABLE playback_history
  ADD COLUMN episode_id BIGINT REFERENCES episodes(id);

CREATE INDEX idx_playback_history_episode_id ON playback_history(episode_id);
