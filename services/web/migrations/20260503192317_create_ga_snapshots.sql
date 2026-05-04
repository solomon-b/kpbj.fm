-- Google Analytics snapshot table for tracking referrers, countries, and cities.
-- Populated by the ga-poller systemd job, which queries the GA Data API hourly.
-- Each row represents the metric value (sessions) for one dimension value
-- aggregated over a closed-open time window [window_start, window_end).

CREATE TABLE ga_snapshots (
  id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  dimension_type TEXT NOT NULL CHECK (dimension_type IN ('source', 'country', 'city')),
  dimension_value TEXT NOT NULL,
  metric_value BIGINT NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  window_start TIMESTAMPTZ NOT NULL,
  window_end TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_ga_snapshots_recorded_at ON ga_snapshots(recorded_at DESC);
CREATE INDEX idx_ga_snapshots_window ON ga_snapshots(dimension_type, window_start, window_end);
