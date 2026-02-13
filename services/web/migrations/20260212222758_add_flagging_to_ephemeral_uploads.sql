CREATE TYPE flag_reason AS ENUM (
  'inappropriate_content',
  'poor_audio_quality',
  'copyright_concern'
);

ALTER TABLE ephemeral_uploads ADD COLUMN flagged_at TIMESTAMPTZ;
ALTER TABLE ephemeral_uploads ADD COLUMN flagged_by BIGINT REFERENCES users(id);
ALTER TABLE ephemeral_uploads ADD COLUMN flag_reason flag_reason;

CREATE INDEX idx_ephemeral_uploads_unflagged
  ON ephemeral_uploads(created_at DESC) WHERE flagged_at IS NULL;
