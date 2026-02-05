-- Sanitize PII for User and Host Accounts
-- Updates email, password, and names for non-privileged accounts.
-- Staff and Admin accounts retain their real credentials.
--
-- This file is designed to be run via psql with variable substitution:
--   psql -v password_hash="'$SANITIZED_PASSWORD_HASH'" -f sanitize-pii.sql
--
-- Or embedded in a heredoc with the password hash substituted.

-- Update user credentials for User and Host roles
UPDATE users u
SET
  email = 'user' || u.id || '@example.com',
  password = :password_hash
FROM user_metadata um
WHERE um.user_id = u.id
  AND um.user_role IN ('User', 'Host');

-- Update user metadata for User and Host roles
UPDATE user_metadata
SET
  display_name = 'user' || user_id,
  full_name = 'Test User ' || user_id
WHERE user_role IN ('User', 'Host');

-- Clear all active sessions
TRUNCATE server_sessions;
