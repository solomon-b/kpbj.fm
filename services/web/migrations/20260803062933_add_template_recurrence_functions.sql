-- Recurrence helpers for schedule_templates.
--
-- Keep each body a single IMMUTABLE expression so Postgres inlines it. Do not
-- cast the enum to text. That makes the body non-immutable and stops the
-- inlining, with no change to any result.

CREATE FUNCTION day_of_week_num(p_day day_of_week) RETURNS BIGINT
LANGUAGE SQL
IMMUTABLE
AS $$
  SELECT CASE p_day
    WHEN 'sunday' THEN 0
    WHEN 'monday' THEN 1
    WHEN 'tuesday' THEN 2
    WHEN 'wednesday' THEN 3
    WHEN 'thursday' THEN 4
    WHEN 'friday' THEN 5
    WHEN 'saturday' THEN 6
  END;
$$;

COMMENT ON FUNCTION day_of_week_num(day_of_week) IS
  'Day number for a day_of_week enum value. Matches EXTRACT(DOW FROM date).';

-- A NULL p_weeks means every week. Any other NULL argument returns false rather
-- than NULL, so callers can use the result inside NOT and CASE. A one-time
-- template holds NULL in both recurrence columns and never matches here. Its air
-- date lives in schedule_template_validity.
--
-- The COALESCE does that work. Testing p_day_num for NULL separately would read
-- the argument twice, and inlining then copies the whole day_of_week_num CASE
-- into the plan twice.
CREATE FUNCTION recurrence_airs_on(p_day_num BIGINT, p_weeks BIGINT[], p_date DATE) RETURNS BOOLEAN
LANGUAGE SQL
IMMUTABLE
AS $$
  SELECT COALESCE(
    EXTRACT(DOW FROM p_date)::BIGINT = p_day_num
      AND (
        p_weeks IS NULL
        OR CEIL(EXTRACT(DAY FROM p_date) / 7.0)::BIGINT = ANY(p_weeks)
      ),
    FALSE
  );
$$;

COMMENT ON FUNCTION recurrence_airs_on(BIGINT, BIGINT[], DATE) IS
  'True when a weekly recurrence of p_day_num and p_weeks covers p_date.';
