-- Remove the one-time schedule template shape.
--
-- A one-time template holds NULL in day_of_week and weeks_of_month, and takes its
-- air date from schedule_template_validity instead of from a recurrence. The
-- capability has never been used. Production and development both hold 0 such rows
-- out of 144, counted on 2026-08-05, and no form can create one.
--
-- It also carries a defect. normalizeTemplate in the show edit handler returns
-- Nothing for a NULL day_of_week, which drops the template from both schedulesMatch
-- and removedTemplates. A one-time template is therefore immortal: no schedule edit
-- can ever close it.
--
-- The columns become NOT NULL, which turns the dead shape into an unrepresentable
-- one. Two CHECK constraints lose their purpose at the same time.
--
--   schedule_templates_check                 both columns NULL or both NOT NULL
--   schedule_templates_weeks_of_month_check  its "weeks_of_month IS NULL OR" arm
--
-- recurrence_airs_on keeps its own NULL handling. The arms become unreachable rather
-- than wrong, they cost nothing once the function is inlined, and RecurrenceSpec pins
-- both the calendar behaviour and the query plan.
--
-- To bring one-time templates back, DROP NOT NULL on both columns. That is a
-- metadata-only change, and the SQL function already handles the NULL case.

ALTER TABLE schedule_templates
  ALTER COLUMN day_of_week SET NOT NULL,
  ALTER COLUMN weeks_of_month SET NOT NULL;

-- Both columns are now NOT NULL, so this reads as (TRUE AND TRUE) OR (FALSE AND FALSE).
ALTER TABLE schedule_templates
  DROP CONSTRAINT schedule_templates_check;

ALTER TABLE schedule_templates
  DROP CONSTRAINT schedule_templates_weeks_of_month_check;

ALTER TABLE schedule_templates
  ADD CONSTRAINT schedule_templates_weeks_of_month_check CHECK (
    weeks_of_month <@ ARRAY[1,2,3,4,5]::BIGINT[] AND
    cardinality(weeks_of_month) > 0
  );

COMMENT ON COLUMN schedule_templates.day_of_week IS
  'The weekday this template airs on. Every template recurs; there is no one-time shape.';

COMMENT ON COLUMN schedule_templates.weeks_of_month IS
  'Weeks of the month this template airs on, a non-empty subset of 1 to 5. A show that airs every week holds all five.';
