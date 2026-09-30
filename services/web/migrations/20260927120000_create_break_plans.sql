-- The daily break plan.
--
-- One row in break_plan_days means the plan for that Pacific day exists, even
-- when the day has no breaks. The primary key also lets only one request build
-- a day's plan.
--
-- break_plan_entries holds one row per track in a break, keyed by the break's
-- boundary and the track's position. The break endpoint reads these rows and
-- chooses nothing, so a repeated request returns the same tracks.

CREATE TABLE break_plan_days (
    day        DATE PRIMARY KEY,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE break_plan_entries (
    boundary      TIMESTAMPTZ NOT NULL,
    position      BIGINT NOT NULL CHECK (position >= 0),
    station_id_id BIGINT REFERENCES station_ids(id) ON DELETE CASCADE,
    break_item_id BIGINT REFERENCES break_items(id),
    PRIMARY KEY (boundary, position),
    -- Pacific differs from UTC by whole hours, so a Pacific half-hour is also a
    -- UTC half-hour.
    CONSTRAINT break_plan_entries_on_boundary
        CHECK (EXTRACT(EPOCH FROM boundary)::BIGINT % 1800 = 0),
    CONSTRAINT break_plan_entries_one_ref
        CHECK ((station_id_id IS NULL) <> (break_item_id IS NULL))
);

COMMENT ON TABLE break_plan_days IS 'Pacific days whose break plan has been built';
COMMENT ON TABLE break_plan_entries IS 'One track of one planned break, in play order';
COMMENT ON COLUMN break_plan_entries.station_id_id IS 'Station IDs have no soft delete, so a deleted station ID takes its planned rows with it';
