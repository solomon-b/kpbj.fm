{-# LANGUAGE QuasiQuotes #-}

-- | Database queries for @listener_snapshots@.
--
-- Read-only aggregation queries for the analytics dashboard.
-- The table is populated by the listener-snapshots systemd job.
module Effects.Database.Tables.ListenerSnapshots
  ( -- * Types
    BucketSize (..),

    -- * Queries
    getLatestSnapshot,
    getSnapshotBuckets,
    getPeakInRange,
    getAverageInRange,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Time (UTCTime)
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------
-- Types

-- | Bucket size for time-series aggregation.
data BucketSize = FiveMinute | Hourly | Daily

--------------------------------------------------------------------------------
-- Queries

-- | Get the most recent listener snapshot.
getLatestSnapshot :: Hasql.Statement () (Maybe (Int64, UTCTime))
getLatestSnapshot =
  interp
    False
    [sql|
      SELECT listener_count::bigint, recorded_at
      FROM listener_snapshots
      ORDER BY recorded_at DESC
      LIMIT 1
    |]


-- | Get listener counts bucketed by time interval within a range.
--
-- Returns the average listener count per bucket, ordered by time.
-- The bucket size determines the SQL aggregation:
--
-- * 'FiveMinute' — 5-minute windows (for 24h range)
-- * 'Hourly' — hour windows (for 7d, 30d ranges)
-- * 'Daily' — day windows (for 90d range)
getSnapshotBuckets :: BucketSize -> UTCTime -> UTCTime -> Hasql.Statement () [(Double, UTCTime)]
getSnapshotBuckets bucket start end = case bucket of
  FiveMinute ->
    interp
      False
      [sql|
        SELECT AVG(listener_count)::float8,
               date_trunc('hour', recorded_at)
                 + (EXTRACT(MINUTE FROM recorded_at)::int / 5) * INTERVAL '5 minutes'
                 AS bucket
        FROM listener_snapshots
        WHERE recorded_at >= #{start} AND recorded_at < #{end}
        GROUP BY bucket
        ORDER BY bucket
      |]
  Hourly ->
    interp
      False
      [sql|
        SELECT AVG(listener_count)::float8,
               date_trunc('hour', recorded_at) AS bucket
        FROM listener_snapshots
        WHERE recorded_at >= #{start} AND recorded_at < #{end}
        GROUP BY bucket
        ORDER BY bucket
      |]
  Daily ->
    interp
      False
      [sql|
        SELECT AVG(listener_count)::float8,
               date_trunc('day', recorded_at) AS bucket
        FROM listener_snapshots
        WHERE recorded_at >= #{start} AND recorded_at < #{end}
        GROUP BY bucket
        ORDER BY bucket
      |]


-- | Get peak listener count within a range.
--
-- Returns 'Nothing' when the range contains no snapshots.
getPeakInRange :: UTCTime -> UTCTime -> Hasql.Statement () (Maybe Int64)
getPeakInRange start end =
  fmap getOneColumn
    <$> interp
      False
      [sql|
        SELECT MAX(listener_count)::bigint
        FROM listener_snapshots
        WHERE recorded_at >= #{start} AND recorded_at < #{end}
      |]


-- | Get average listener count within a range.
--
-- Returns 'Nothing' when the range contains no snapshots.
getAverageInRange :: UTCTime -> UTCTime -> Hasql.Statement () (Maybe Double)
getAverageInRange start end =
  fmap getOneColumn
    <$> interp
      False
      [sql|
        SELECT AVG(listener_count)::float8
        FROM listener_snapshots
        WHERE recorded_at >= #{start} AND recorded_at < #{end}
      |]
