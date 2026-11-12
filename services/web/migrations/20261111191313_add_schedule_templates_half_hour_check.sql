-- Keep every slot time of schedule_templates on the hour or the half hour.
--
-- The playout timeline puts a segment edge at each slot start and at each hour
-- and half hour. A slot end is not an edge. A slot that ended off the grid would
-- let its episode play past its end, and no station break would air before the
-- next show. breakKindsBetween also looks only at half-hour boundaries. Every
-- segment is thus at least 30 minutes long, and Liquidsoap depends on that.
--
-- The schedule editor offers only :00 and :30, and the durations are 30, 60, or
-- 120 minutes. parseScheduleSlot now refuses other start and replay times. This
-- CHECK covers a row that comes from somewhere else. No prod row broke the rule
-- when this migration was written.
--
-- EXTRACT(EPOCH FROM time) gives the seconds since midnight, with the fraction.
-- So the test also refuses a time with seconds.

ALTER TABLE schedule_templates
  ADD CONSTRAINT schedule_templates_half_hour_times CHECK (
    EXTRACT(EPOCH FROM start_time) % 1800 = 0
    AND EXTRACT(EPOCH FROM end_time) % 1800 = 0
    AND (replay_start_time IS NULL OR EXTRACT(EPOCH FROM replay_start_time) % 1800 = 0)
  );
