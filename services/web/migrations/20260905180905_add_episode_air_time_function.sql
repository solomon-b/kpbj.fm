-- The instant an episode airs, from its date and its template's local start time.
--
-- An episode's air time is described twice: episodes.scheduled_at holds an
-- instant, and the template holds the local start_time that instant was computed
-- from. The two can disagree, which is the defect templateAirTimeOn exists to
-- prevent at the two writers. Removing the duplication means every reader that
-- needs the instant has to derive it, and four statements in Episodes.hs need it
-- to decide which episodes a schedule change may touch.
--
-- Keep the body a single expression so Postgres inlines it, the same rule
-- day_of_week_num and recurrence_airs_on follow. The plan then shows the
-- expression rather than a function call, and the shape does not change.
--
-- timezone(text, timestamp) is IMMUTABLE, so this is too. The timezone is a
-- parameter rather than a literal because templateAirTimeOn reads st.timezone,
-- and this has to agree with it. Every template is currently
-- America/Los_Angeles.

CREATE FUNCTION episode_air_time(p_air_date DATE, p_start_time TIME, p_timezone TEXT)
RETURNS TIMESTAMPTZ
LANGUAGE SQL
IMMUTABLE
AS $$
  SELECT (p_air_date + p_start_time) AT TIME ZONE p_timezone;
$$;

COMMENT ON FUNCTION episode_air_time(DATE, TIME, TEXT) IS
  'The instant an episode airs, from its air date and its template''s local start time.';
