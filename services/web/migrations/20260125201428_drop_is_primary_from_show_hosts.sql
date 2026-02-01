-- Drop the is_primary column from show_hosts.
-- The "primary host" concept has been removed - schedule now shows all hosts.

ALTER TABLE show_hosts DROP COLUMN is_primary;
