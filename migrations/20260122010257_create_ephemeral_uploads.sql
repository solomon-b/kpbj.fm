-- Ephemeral uploads table for random audio content played at night.
-- Hosts can upload short audio clips to be played during off-hours.

CREATE TABLE ephemeral_uploads (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    audio_file_path TEXT NOT NULL,
    mime_type TEXT NOT NULL,
    file_size BIGINT NOT NULL,
    creator_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for fetching ephemeral uploads by creator
CREATE INDEX idx_ephemeral_uploads_creator ON ephemeral_uploads(creator_id);

-- Index for ordering by creation date (newest first)
CREATE INDEX idx_ephemeral_uploads_created_at ON ephemeral_uploads(created_at DESC);

COMMENT ON TABLE ephemeral_uploads IS 'Ephemeral audio clips uploaded by hosts for nighttime playback';
COMMENT ON COLUMN ephemeral_uploads.title IS 'Display name for the ephemeral upload';
COMMENT ON COLUMN ephemeral_uploads.audio_file_path IS 'Path to the audio file in storage (local or S3)';
COMMENT ON COLUMN ephemeral_uploads.mime_type IS 'MIME type of the audio file';
COMMENT ON COLUMN ephemeral_uploads.file_size IS 'Size of the audio file in bytes';
COMMENT ON COLUMN ephemeral_uploads.creator_id IS 'User who uploaded this ephemeral clip';
