-- Forbid the '24:00:00' time-of-day in schedule_templates.
--
-- getCurrentlyAiringEpisode classifies a slot as a standard (non-overnight) show
-- when end_time > start_time. A slot stored with end_time = '24:00:00' takes that
-- branch, where the airing window is
--   time_now < LEAST(end_time, (start_time + duration)::TIME)
-- and (start_time + duration)::TIME wraps to '00:00:00', collapsing the window so
-- the slot dead-airs for its entire duration.
--
-- The schedule editor never produces '24:00:00': end times are computed with
-- addMinutesToTimeOfDay, which wraps modulo 24, so a midnight end is stored as
-- '00:00:00' (an overnight slot, end_time <= start_time) and airs correctly. Only
-- hand-seeded rows ever used '24:00:00'. Normalize any such rows, then add a CHECK
-- so it can never be stored again and the standard-vs-overnight split stays well-defined.

UPDATE schedule_templates SET end_time = '00:00:00' WHERE end_time = '24:00:00';
UPDATE schedule_templates SET start_time = '00:00:00' WHERE start_time = '24:00:00';
UPDATE schedule_templates SET replay_start_time = '00:00:00' WHERE replay_start_time = '24:00:00';

ALTER TABLE schedule_templates
  ADD CONSTRAINT schedule_templates_no_24h_time CHECK (
    start_time < TIME '24:00:00'
    AND end_time < TIME '24:00:00'
    AND (replay_start_time IS NULL OR replay_start_time < TIME '24:00:00')
  );
