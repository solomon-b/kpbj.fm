-- episodes.scheduled_at held an instant whose time of day carried no
-- information. The airing window comes from the template's start_time and
-- end_time, and getCurrentlyAiringEpisodes read only the date. Two values
-- described one air time and could disagree, which is the defect
-- templateAirTimeOn exists to prevent at the two writers.
--
-- Every reader that needs the instant now derives it through episode_air_time,
-- so nothing reads the time component any more. The column keeps the date
-- alone, and the template keeps the time.
--
-- The conversion is lossless. On development all 482 scheduled rows hold a time
-- of day equal to their template's start_time, so the USING clause discards
-- nothing. Production is checked the same way before this runs.
--
-- Two indexes reference the column and PostgreSQL rebuilds both during the type
-- change. episodes_schedule_consistency references it by identity, so it
-- follows the rename with no restatement. No view, rule, default or generated
-- expression depends on it, and episode_number_trigger does not read it.
--
-- unique_episode_air_date becomes one live episode per show per date, which is
-- the rule the review wanted and which needed a trigger to express while the
-- column held a timestamp.
--
-- Reversing this recomputes the instant from the template rather than restoring
-- a stored value. That is exact for every row whose template has not had its
-- start_time edited since.

ALTER TABLE episodes
  ALTER COLUMN scheduled_at TYPE DATE
  USING (scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE;

ALTER TABLE episodes RENAME COLUMN scheduled_at TO air_date;

ALTER INDEX idx_episodes_scheduled_at RENAME TO idx_episodes_air_date;
ALTER INDEX unique_episode_scheduled_at RENAME TO unique_episode_air_date;

COMMENT ON COLUMN episodes.air_date IS
  'The Pacific date this episode airs on. The time comes from its schedule template.';
