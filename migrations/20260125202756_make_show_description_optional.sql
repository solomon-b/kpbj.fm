-- Make show description optional
ALTER TABLE shows ALTER COLUMN description DROP NOT NULL;
