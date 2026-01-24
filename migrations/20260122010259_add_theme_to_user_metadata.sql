-- Add theme enum and column to user_metadata table
CREATE TYPE theme_name AS ENUM ('Default', 'Solarized');
ALTER TABLE user_metadata ADD COLUMN theme theme_name NOT NULL DEFAULT 'Default';
