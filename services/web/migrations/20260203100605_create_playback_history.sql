-- Playback history table for logging what has been played on the stream.
-- Populated by Liquidsoap's source.on_track callback via POST /api/playout/played.

CREATE TABLE playback_history (
  id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  title TEXT NOT NULL,
  artist TEXT,
  source_type TEXT NOT NULL CHECK (source_type IN ('episode', 'ephemeral')),
  source_url TEXT NOT NULL,
  started_at TIMESTAMPTZ NOT NULL
);

-- Index for efficient retrieval of recent history
CREATE INDEX idx_playback_history_started_at ON playback_history(started_at DESC);
