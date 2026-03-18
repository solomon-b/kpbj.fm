-- Replace airs_twice_daily BOOLEAN with replay_start_time TIME
-- Allows configurable replay times instead of hardcoded +12h offset.

-- 1. Add replay_start_time column (nullable TIME — NULL means no replay)
ALTER TABLE schedule_templates ADD COLUMN replay_start_time TIME;

-- 2a. PM primaries (start_time >= 12:00): swap so AM becomes primary, PM becomes replay
--     e.g., 20:00-22:00 primary → 08:00-10:00 primary, replay at 20:00
UPDATE schedule_templates
SET
  replay_start_time = start_time,
  start_time = (start_time + INTERVAL '12 hours')::TIME,
  end_time = (end_time + INTERVAL '12 hours')::TIME
WHERE airs_twice_daily = TRUE
  AND start_time >= TIME '12:00:00';

-- 2b. AM primaries (start_time < 12:00): keep primary as-is, replay is +12h (already PM)
--     e.g., 08:00-10:00 primary → replay at 20:00
UPDATE schedule_templates
SET
  replay_start_time = (start_time + INTERVAL '12 hours')::TIME
WHERE airs_twice_daily = TRUE
  AND start_time < TIME '12:00:00';

-- 3. Drop the old column
ALTER TABLE schedule_templates DROP COLUMN airs_twice_daily;

-- 4. Add comment
COMMENT ON COLUMN schedule_templates.replay_start_time IS
  'When not NULL, show replays at this time of day (same calendar day). Must be >= end_time.';
