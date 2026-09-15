-- Add break_item_audio to staged_uploads upload_type CHECK constraint
-- Needed for PSA and advertisement spot uploads in the dashboard.

ALTER TABLE staged_uploads DROP CONSTRAINT IF EXISTS staged_uploads_upload_type_check;

ALTER TABLE staged_uploads ADD CONSTRAINT staged_uploads_upload_type_check
    CHECK (upload_type IN ('episode_audio', 'episode_artwork', 'show_logo', 'show_banner', 'blog_image', 'event_image', 'user_avatar', 'station_id_audio', 'ephemeral_audio', 'break_item_audio'));
