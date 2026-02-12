-- Add description column to ephemeral_uploads table.
-- Backfills existing rows with their title as description.

ALTER TABLE ephemeral_uploads ADD COLUMN description TEXT NOT NULL DEFAULT '';

UPDATE ephemeral_uploads SET description = title WHERE description = '';

ALTER TABLE ephemeral_uploads ALTER COLUMN description DROP DEFAULT;

COMMENT ON COLUMN ephemeral_uploads.description IS 'Description of the ephemeral upload content, minimum ~80 characters (2 sentences).';
