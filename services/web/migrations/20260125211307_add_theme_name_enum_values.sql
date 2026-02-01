-- Convert theme column from enum to text for flexibility

-- First remove the default (which references the enum type)
ALTER TABLE user_metadata ALTER COLUMN theme DROP DEFAULT;

-- Convert the column to text
ALTER TABLE user_metadata ALTER COLUMN theme TYPE text USING theme::text;

-- Set a new text default
ALTER TABLE user_metadata ALTER COLUMN theme SET DEFAULT 'Default';

-- Now we can drop the enum type
DROP TYPE theme_name;
