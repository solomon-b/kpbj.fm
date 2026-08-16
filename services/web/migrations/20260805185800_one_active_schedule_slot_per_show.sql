-- Allow a show only one open schedule slot at a time.
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

-- Some ended windows overlap an active one, from a slot that moved to a new time
-- while the outgoing window closed a few days after its replacement opened. Such a
-- window records nothing, so it collapses to an empty range, which overlaps nothing
-- and which the active-template query already excludes. cancelPendingSchedule retires
-- a pending window the same way.
--
-- A window that aired an episode is left alone. Collapsing one would leave the episode
-- pointing at a template that covers no date, and the airing query would then refuse
-- it. Production held 13 such episodes in August 2026, from a separate defect, and
-- restoring them is what this predicate protects.
--
-- bertha-radio is the reason the constraint below is scoped to open windows. It aired
-- a one-off Wednesday 09:00 during a Tuesday 07:00 run, so template 260 overlaps 245
-- and sits wholly inside it. Neither side can be truncated, and the station really did
-- hold two slots that week.
UPDATE schedule_template_validity v
SET effective_until = v.effective_from
FROM schedule_templates vt
WHERE vt.id = v.template_id
  AND v.effective_until IS NOT NULL
  AND v.effective_until <= CURRENT_DATE
  AND NOT EXISTS (
    SELECT 1 FROM episodes e WHERE e.schedule_template_id = v.template_id
  )
  AND EXISTS (
    SELECT 1
    FROM schedule_template_validity o
    JOIN schedule_templates ot ON ot.id = o.template_id
    WHERE ot.show_id = vt.show_id
      AND o.id <> v.id
      AND daterange(o.effective_from, o.effective_until)
       && daterange(v.effective_from, v.effective_until)
  );

-- The rule covers the open windows only. An open window is the slot a show holds now,
-- and two of them is the state that double-books a time. A closed window is a record
-- of what the show used to hold, and the station has held two slots at once, so
-- comparing closed windows would reject the truth.
--
-- This still catches a deferred double-book. A schedule change writes the replacement
-- as [startDate, NULL), so two pending changes are two open windows and collide. An
-- ordinary change writes effective_until = startDate on the outgoing template and
-- effective_from = startDate on its replacement, which are adjacent and leave one open
-- window.
--
-- daterange(x, NULL) is [x, infinity), so every row the predicate admits shares the
-- same infinite upper bound and any two of them for one show overlap.
--
-- If two open windows survive the statement above, this fails and the migration aborts.
ALTER TABLE schedule_template_validity
  ADD CONSTRAINT one_active_slot_per_show
  EXCLUDE USING gist (
    int8range(show_id, show_id, '[]') WITH &&,
    daterange(effective_from, effective_until) WITH &&
  ) WHERE (effective_until IS NULL);

COMMENT ON CONSTRAINT one_active_slot_per_show ON schedule_template_validity IS
  'A show holds at most one open schedule slot. Closed windows are history and may overlap.';
