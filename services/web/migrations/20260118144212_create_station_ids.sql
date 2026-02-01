-- Station IDs table for short station identification audio clips.
-- Hosts can upload these for use during broadcasts.

CREATE TABLE station_ids (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    audio_file_path TEXT NOT NULL,
    mime_type TEXT NOT NULL,
    file_size BIGINT NOT NULL,
    creator_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for fetching station IDs by creator
CREATE INDEX idx_station_ids_creator ON station_ids(creator_id);

-- Index for ordering by creation date (newest first)
CREATE INDEX idx_station_ids_created_at ON station_ids(created_at DESC);

COMMENT ON TABLE station_ids IS 'Station identification audio clips uploaded by hosts';
COMMENT ON COLUMN station_ids.title IS 'Display name for the station ID';
COMMENT ON COLUMN station_ids.audio_file_path IS 'Path to the audio file in storage (local or S3)';
COMMENT ON COLUMN station_ids.mime_type IS 'MIME type of the audio file';
COMMENT ON COLUMN station_ids.file_size IS 'Size of the audio file in bytes';
COMMENT ON COLUMN station_ids.creator_id IS 'User who uploaded this station ID';
