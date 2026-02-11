ALTER TABLE events ADD COLUMN featured_on_homepage BOOLEAN NOT NULL DEFAULT FALSE;

-- Partial unique index: at most one event can be featured at a time
CREATE UNIQUE INDEX idx_events_featured_on_homepage
  ON events (featured_on_homepage) WHERE featured_on_homepage = TRUE;
