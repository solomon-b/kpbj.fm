-- Remove episode status column and switch to soft deletes via deleted_at timestamp.
-- Episodes are now implicitly published when created. Soft delete via deleted_at.

-- Add deleted_at column for soft deletes
ALTER TABLE episodes ADD COLUMN deleted_at TIMESTAMPTZ DEFAULT NULL;

-- Migrate deleted episodes (set deleted_at to updated_at for previously deleted)
UPDATE episodes SET deleted_at = updated_at WHERE status = 'deleted';

-- Convert drafts to published (set published_at if null)
UPDATE episodes SET published_at = COALESCE(published_at, NOW()) WHERE status = 'draft';

-- Drop the status column and related constraints/indexes
DROP INDEX IF EXISTS idx_episodes_status;
ALTER TABLE episodes DROP COLUMN status;

-- Drop the episode_status enum type
DROP TYPE IF EXISTS episode_status;
