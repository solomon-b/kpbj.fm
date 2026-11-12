-- Record the planned start of each airing in the playout timeline.
--
-- Liquidsoap can restart while an item plays. It then posts the same airing
-- again. The planned start from the timeline and the audio URL name one
-- airing. A unique index on the two lets the insert skip the second post, so
-- the airing gets one history row, and a break item airing counts once. A
-- second airing of the same item has a different planned start, so it still
-- gets its own row.
--
-- The URL is part of the key because a schedule change can put a different
-- item at the same planned start, for example an episode that is uploaded
-- late. Both airings then keep their rows.

ALTER TABLE playback_history ADD COLUMN planned_start TIMESTAMPTZ;

COMMENT ON COLUMN playback_history.planned_start IS 'Planned start of the item in the playout timeline. Liquidsoap sends it for every item. NULL for rows from older clients';

CREATE UNIQUE INDEX idx_playback_history_source_url_planned_start
  ON playback_history (source_url, planned_start)
  WHERE planned_start IS NOT NULL;
