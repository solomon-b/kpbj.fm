-- Add email verification columns to users table
--
-- Adds a boolean flag to track whether users have verified their email address
-- and a timestamp for when verification occurred.

ALTER TABLE users
ADD COLUMN email_verified BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE users
ADD COLUMN email_verified_at TIMESTAMP WITH TIME ZONE;

-- Index for finding unverified users (useful for cleanup jobs)
CREATE INDEX idx_users_email_verified ON users(email_verified) WHERE email_verified = FALSE;
