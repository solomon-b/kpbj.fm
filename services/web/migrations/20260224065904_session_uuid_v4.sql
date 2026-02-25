-- Switch session ID default from uuid_generate_v1mc() (time-based, leaks server MAC
-- and creation timestamp) to gen_random_uuid() (v4, CSPRNG-backed).
ALTER TABLE server_sessions ALTER COLUMN id SET DEFAULT gen_random_uuid();
DROP EXTENSION IF EXISTS "uuid-ossp";

-- Add idle timeout tracking.
ALTER TABLE server_sessions
  ADD COLUMN last_activity_at TIMESTAMPTZ NOT NULL DEFAULT now();

-- Drop single-session trigger (replaced by CTE in insertServerSession
-- that caps active sessions at 5 per user).
DROP TRIGGER IF EXISTS check_only_one_active_session ON server_sessions;
DROP FUNCTION IF EXISTS check_only_one_active_session();

-- Expire all existing sessions to force re-login with secure IDs.
TRUNCATE server_sessions;
