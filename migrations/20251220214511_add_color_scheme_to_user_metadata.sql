-- Add color_scheme enum and column to user_metadata table

-- Create the color_scheme enum type
CREATE TYPE color_scheme AS ENUM ('Automatic', 'LightMode', 'DarkMode');

-- Add color_scheme column to user_metadata with default 'Automatic'
ALTER TABLE user_metadata
ADD COLUMN color_scheme color_scheme NOT NULL DEFAULT 'Automatic';
