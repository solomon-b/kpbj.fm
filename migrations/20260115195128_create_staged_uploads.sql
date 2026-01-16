-- Staged uploads table for YouTube/Bandcamp-style background file uploads.
-- Files are uploaded immediately on selection and stored with a unique token.
-- When the form is submitted, the token is used to claim the upload and associate it with the entity.
-- Unclaimed uploads are cleaned up by a background job after expiration.

CREATE TABLE staged_uploads (
    id BIGSERIAL PRIMARY KEY,
    token TEXT NOT NULL UNIQUE,
    user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    original_name TEXT NOT NULL,
    storage_path TEXT NOT NULL,
    mime_type TEXT NOT NULL,
    file_size BIGINT NOT NULL,
    upload_type TEXT NOT NULL CHECK (upload_type IN ('episode_audio', 'episode_artwork', 'show_logo', 'show_banner', 'blog_image', 'event_image', 'user_avatar')),
    status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'claimed', 'expired')),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    claimed_at TIMESTAMPTZ,
    expires_at TIMESTAMPTZ NOT NULL DEFAULT (NOW() + INTERVAL '24 hours')
);

-- Index for token lookup (most common query)
CREATE INDEX idx_staged_uploads_token ON staged_uploads(token);

-- Index for finding expired uploads to clean up
CREATE INDEX idx_staged_uploads_expires ON staged_uploads(expires_at) WHERE status = 'pending';

-- Index for finding uploads by user (for debugging/admin)
CREATE INDEX idx_staged_uploads_user ON staged_uploads(user_id);

COMMENT ON TABLE staged_uploads IS 'Temporary storage for background file uploads before form submission';
COMMENT ON COLUMN staged_uploads.token IS 'Unique token returned to client for claiming the upload';
COMMENT ON COLUMN staged_uploads.upload_type IS 'Type of upload determines validation rules and storage location';
COMMENT ON COLUMN staged_uploads.status IS 'pending = awaiting claim, claimed = associated with entity, expired = cleanup candidate';
