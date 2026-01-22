-- Add ephemeral_audio to staged_uploads upload_type CHECK constraint

-- Drop the existing CHECK constraint
ALTER TABLE staged_uploads DROP CONSTRAINT IF EXISTS staged_uploads_upload_type_check;

-- Add new CHECK constraint with ephemeral_audio included
ALTER TABLE staged_uploads ADD CONSTRAINT staged_uploads_upload_type_check
    CHECK (upload_type IN ('episode_audio', 'episode_artwork', 'show_logo', 'show_banner', 'blog_image', 'event_image', 'user_avatar', 'station_id_audio', 'ephemeral_audio'));
