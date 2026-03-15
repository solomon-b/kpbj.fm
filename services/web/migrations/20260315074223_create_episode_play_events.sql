-- Episode play events table for tracking archive plays via the browser audio player.
-- Populated by POST /api/analytics/episode-play when a user plays an archived episode.

CREATE TABLE episode_play_events (
  id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  episode_id BIGINT NOT NULL REFERENCES episodes(id),
  played_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_episode_play_events_episode_id ON episode_play_events(episode_id);
CREATE INDEX idx_episode_play_events_played_at ON episode_play_events(played_at DESC);
