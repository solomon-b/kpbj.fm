-- Reject an empty weeks_of_month in schedule_templates.
--
-- The original CHECK, written in migration 20250927022359, was
--
--   weeks_of_month IS NULL OR (
--     weeks_of_month <@ ARRAY[1,2,3,4,5]::BIGINT[] AND
--     array_length(weeks_of_month, 1) > 0
--   )
--
-- and it accepts '{}'. The subset test is true for the empty array, and
-- array_length of an empty array is NULL rather than 0, so the conjunction is
-- NULL, the whole expression is NULL, and Postgres accepts a CHECK that evaluates
-- to NULL. cardinality returns 0 for the empty array, so it gives false and the
-- row is rejected.
--
-- An empty weeks_of_month describes a slot that never airs. recurrence_airs_on
-- compares the week of the month against ANY(weeks_of_month), and no week belongs
-- to an empty set. It is also invisible to both conflict checks, which test week
-- membership the same way. So the show holds a slot, broadcasts nothing, and stops
-- nobody from booking over it.
--
-- The schedule editor cannot produce one. It emits [1,2,3,4,5], [1,3], [2,4], or a
-- single week, and parseScheduleSlot now rejects an empty list before any write.
-- Only a hand-written POST ever reached the database with one.
--
-- No data step. Production holds 0 such rows out of 144, and development 0 out of
-- 192. Both were counted on 2026-08-05.

ALTER TABLE schedule_templates
  DROP CONSTRAINT schedule_templates_weeks_of_month_check;

ALTER TABLE schedule_templates
  ADD CONSTRAINT schedule_templates_weeks_of_month_check CHECK (
    weeks_of_month IS NULL OR (
      weeks_of_month <@ ARRAY[1,2,3,4,5]::BIGINT[] AND
      cardinality(weeks_of_month) > 0
    )
  );
