-- Remove title and slug columns from episodes table
-- Episode identification will be by show name + episode number instead

ALTER TABLE episodes DROP COLUMN title;
ALTER TABLE episodes DROP COLUMN slug;

-- Drop the slug index
DROP INDEX IF EXISTS idx_episodes_slug;
