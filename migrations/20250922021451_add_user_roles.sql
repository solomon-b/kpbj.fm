-- Create user_role domain with enum values
CREATE DOMAIN user_role AS TEXT CHECK (VALUE IN ('User', 'Host', 'Staff', 'Admin'));

-- Add user_role column to user_metadata table
ALTER TABLE user_metadata ADD COLUMN user_role user_role NOT NULL DEFAULT 'User';

-- Migrate existing is_admin data to new role system
UPDATE user_metadata SET user_role = 'Admin' WHERE is_admin = TRUE;

-- Drop the old is_admin column
ALTER TABLE user_metadata DROP COLUMN is_admin;
