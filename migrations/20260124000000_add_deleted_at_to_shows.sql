-- Add soft delete support to shows table
-- NULL = active, non-NULL timestamp = deleted

ALTER TABLE shows ADD COLUMN deleted_at TIMESTAMPTZ DEFAULT NULL;

-- Partial index for efficient queries on active (non-deleted) shows
CREATE INDEX idx_shows_deleted_at ON shows(deleted_at) WHERE deleted_at IS NULL;

COMMENT ON COLUMN shows.deleted_at IS 'Soft delete timestamp - NULL means active, non-NULL means deleted.';
