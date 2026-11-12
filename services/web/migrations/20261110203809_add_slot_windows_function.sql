-- The air windows of one schedule template on one air date.
--
-- Three queries need the instants that a slot opens and closes:
-- getAiringEpisodes, getSlotWindowsBetween, and breakKindsBetween. Each one
-- held its own copy of this arithmetic. This function is now the only copy.
--
-- The function returns one row for the primary window. It returns a second row
-- for the replay window when p_replay_start_time is set. A replay runs for the
-- same length as its primary.
--
-- The rules:
--
-- * A slot that closes at or before it opens runs onto the next date. So an
--   overnight slot wraps midnight.
-- * The zone is America/Los_Angeles. Every template uses it, and the callers
--   scan their air dates in it.
-- * On the fall-back date a local time from 01:00 to 01:59 names two instants,
--   an hour apart. AT TIME ZONE alone returns the later one. LEAST against the
--   same conversion an hour earlier returns the first one. So a slot opens when
--   the clock first reads its start time. On every other local time of the year
--   both arms give the same instant.
-- * A slot that opens inside the spring-forward gap gives an empty or inverted
--   window. The function returns it as it is. Each caller decides what to do
--   with it.
--
-- See the Haddock of getAiringEpisodes for the full reasons.
--
-- Keep the body a single SELECT so Postgres inlines it, the same rule
-- day_of_week_num and recurrence_airs_on follow. timezone(text, timestamp) is
-- IMMUTABLE, so this function is too.

CREATE FUNCTION slot_windows(
  p_air_date DATE,
  p_start_time TIME,
  p_end_time TIME,
  p_replay_start_time TIME
)
RETURNS TABLE (is_replay BOOLEAN, window_start TIMESTAMPTZ, window_end TIMESTAMPTZ)
LANGUAGE SQL
IMMUTABLE
AS $$
  SELECT
    v.is_replay,
    LEAST(
      l.local_start AT TIME ZONE 'America/Los_Angeles',
      (l.local_start - INTERVAL '1 hour') AT TIME ZONE 'America/Los_Angeles'
        + INTERVAL '1 hour'
    ),
    LEAST(
      l.local_end AT TIME ZONE 'America/Los_Angeles',
      (l.local_end - INTERVAL '1 hour') AT TIME ZONE 'America/Los_Angeles'
        + INTERVAL '1 hour'
    )
  FROM (
    VALUES
      (FALSE, p_start_time, p_end_time),
      (
        TRUE,
        p_replay_start_time,
        (p_replay_start_time + CASE
          WHEN p_end_time > p_start_time THEN p_end_time - p_start_time
          ELSE INTERVAL '24 hours' - (p_start_time - p_end_time)
        END)::TIME
      )
  ) AS v(is_replay, start_time, end_time)
  CROSS JOIN LATERAL (
    SELECT
      p_air_date + v.start_time AS local_start,
      (p_air_date + CASE WHEN v.end_time <= v.start_time THEN 1 ELSE 0 END) + v.end_time
        AS local_end
  ) l
  WHERE v.start_time IS NOT NULL;
$$;

COMMENT ON FUNCTION slot_windows(DATE, TIME, TIME, TIME) IS
  'The primary and replay air windows of one schedule template on one air date.';
