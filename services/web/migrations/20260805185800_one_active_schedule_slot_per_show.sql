-- Allow a show only one schedule slot at a time.
--
-- Every one of the 80 shows on production holds exactly one active template, and no
-- show has ever deliberately held two. The schedule editor cannot create a second
-- one: it keeps a single frequency and a single weeks value for the whole show, and
-- serializeForSubmit stamps them onto every slot it emits. A show that somehow held
-- two slots with different weeks lost one of them on the next save, because init()
-- reads the weeks from existing[0] alone.
--
-- The rule moves into the database rather than staying a convention the write path
-- remembers. schedule_template_validity has no show_id, so the constraint needs one,
-- and a composite foreign key stops it drifting from the template's own show_id.
--
-- The exclusion deliberately avoids btree_gist. It is installed on neither database,
-- it is not a trusted extension, and it would need superuser at deploy time. Wrapping
-- show_id in a degenerate int8range gives a GiST-indexable && through the built-in
-- range_ops instead. int8range and daterange are both IMMUTABLE, so they are legal in
-- an index expression.

ALTER TABLE schedule_templates
  ADD CONSTRAINT schedule_templates_id_show_id_key UNIQUE (id, show_id);

ALTER TABLE schedule_template_validity
  ADD COLUMN show_id BIGINT;

UPDATE schedule_template_validity v
  SET show_id = t.show_id
  FROM schedule_templates t
  WHERE t.id = v.template_id;

ALTER TABLE schedule_template_validity
  ALTER COLUMN show_id SET NOT NULL;

ALTER TABLE schedule_template_validity
  DROP CONSTRAINT schedule_template_validity_template_id_fkey;

ALTER TABLE schedule_template_validity
  ADD CONSTRAINT schedule_template_validity_template_show_fkey
    FOREIGN KEY (template_id, show_id)
    REFERENCES schedule_templates (id, show_id) ON DELETE CASCADE;

CREATE INDEX idx_schedule_template_validity_show_id
  ON schedule_template_validity (show_id);

-- Three pairs of windows already overlap, identically on production and development.
--
--   about-time              template 264  Sun 10:00 {4}  2026-02-27 to 2026-03-04
--   in-the-pits-power-hour  template 262  Sat 10:00 {2}  2026-02-27 to 2026-03-03
--   bertha-radio            template 260  Wed 09:00 {4}  2026-02-20 to 2026-02-27
--
-- The first two are one slot moving to an earlier time, where the outgoing window
-- closed a few days after its replacement opened. Neither aired an episode. The third
-- is the only second slot production has ever held, and it aired one episode.
--
-- In each pair exactly one window has already ended and the other is still active, so
-- one statement covers all three without naming an id. The ended window collapses to
-- an empty range, which overlaps nothing and which the active-template query already
-- excludes. cancelPendingSchedule retires a pending window the same way.
--
-- The episode on template 260 is unaffected. Its date, audio, archive position, and
-- scheduled_at all live on the episode row. What changes is the public schedule for
-- the week of 2026-02-23, which no longer lists bertha-radio on the Wednesday.
UPDATE schedule_template_validity v
SET effective_until = v.effective_from
FROM schedule_templates vt
WHERE vt.id = v.template_id
  AND v.effective_until IS NOT NULL
  AND v.effective_until <= CURRENT_DATE
  AND EXISTS (
    SELECT 1
    FROM schedule_template_validity o
    JOIN schedule_templates ot ON ot.id = o.template_id
    WHERE ot.show_id = vt.show_id
      AND o.id <> v.id
      AND daterange(o.effective_from, o.effective_until)
       && daterange(v.effective_from, v.effective_until)
  );

-- daterange(x, NULL) is [x, infinity), so an open-ended window works unchanged and two
-- open-ended windows for one show now collide. A normal schedule change writes
-- effective_until = startDate on the outgoing template and effective_from = startDate
-- on its replacement, which are adjacent rather than overlapping.
--
-- If any overlap survives the statement above, this fails and the migration aborts.
ALTER TABLE schedule_template_validity
  ADD CONSTRAINT one_active_slot_per_show
  EXCLUDE USING gist (
    int8range(show_id, show_id, '[]') WITH &&,
    daterange(effective_from, effective_until) WITH &&
  );

COMMENT ON CONSTRAINT one_active_slot_per_show ON schedule_template_validity IS
  'A show holds at most one schedule slot on any given date.';
