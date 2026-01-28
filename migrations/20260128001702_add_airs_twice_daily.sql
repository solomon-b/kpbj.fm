-- Add airs_twice_daily column to schedule_templates
-- When TRUE, show airs twice daily - at start_time and 12 hours later.

ALTER TABLE schedule_templates
ADD COLUMN airs_twice_daily BOOLEAN NOT NULL DEFAULT FALSE;

COMMENT ON COLUMN schedule_templates.airs_twice_daily IS
  'When TRUE, show airs twice daily - at start_time and 12 hours later.';
