-- Stream settings table for configuring the Icecast stream URL and metadata URL
-- Singleton table (only one row allowed via CHECK constraint)
CREATE TABLE stream_settings (
  id BIGINT PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  stream_url TEXT NOT NULL,
  metadata_url TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_by BIGINT REFERENCES users(id)
);

-- Seed with current values
INSERT INTO stream_settings (stream_url, metadata_url) VALUES (
  'https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3',
  'https://kpbj.hasnoskills.com/api/nowplaying/kpbj_test_station'
);
