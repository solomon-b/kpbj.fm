-- Listener snapshot table for tracking Icecast listener counts over time.
-- Populated by the listener-snapshots batch job on a systemd timer.

CREATE TABLE listener_snapshots (
  id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  listener_count INTEGER NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_listener_snapshots_recorded_at ON listener_snapshots(recorded_at DESC);
