{-# LANGUAGE QuasiQuotes #-}

-- | Database queries for @listener_snapshots@.
--
-- Read-only aggregation queries for the analytics dashboard.
-- The table is populated by the listener-snapshots systemd job.
module Effects.Database.Tables.ListenerSnapshots
  ( -- * Types
    BucketSize (..),
    HeatmapRow (..),

    -- * Queries
    getSnapshotBuckets,
    getPeakInRange,
    getAverageInRange,
    getHourlyHeatmap,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int32, Int64)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------
-- Types

-- | Bucket size for time-series aggregation.
data BucketSize = FiveMinute | Hourly | Daily
  deriving stock (Eq, Show)

-- | The average listener count for one weekday and hour in station time.
data HeatmapRow = HeatmapRow
  { -- | ISO weekday, from 1 (Monday) to 7 (Sunday)
    weekday :: Int32,
    -- | Hour of the day, from 0 to 23
    hour :: Int32,
    avgListeners :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance HeatmapRow)

--------------------------------------------------------------------------------
-- Queries

-- | Get listener counts bucketed by time interval within a range.
--
-- Returns the average listener count per bucket, ordered by time.
-- The bucket size determines the SQL aggregation:
--
-- * 'FiveMinute' — 5-minute windows (for 24h range)
-- * 'Hourly' — hour windows (for 7d, 30d ranges)
-- * 'Daily' — day windows (for 90d range)
--
-- Each bucket starts at a time in station time (@America/Los_Angeles@), so a
-- day bucket starts at midnight Pacific.
getSnapshotBuckets :: BucketSize -> UTCTime -> UTCTime -> Hasql.Statement () [(Double, UTCTime)]
getSnapshotBuckets bucket start end = case bucket of
  FiveMinute ->
    interp
      False
      [sql|
        SELECT AVG(listener_count)::float8,
               date_trunc('hour', recorded_at, 'America/Los_Angeles')
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
               date_trunc('hour', recorded_at, 'America/Los_Angeles') AS bucket
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
               date_trunc('day', recorded_at, 'America/Los_Angeles') AS bucket
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

-- | Get the average listener count for each weekday and hour within a range.
--
-- The weekday and the hour are in station time (@America/Los_Angeles@). A
-- weekday and hour with no snapshots has no row.
getHourlyHeatmap :: UTCTime -> UTCTime -> Hasql.Statement () [HeatmapRow]
getHourlyHeatmap start end =
  interp
    False
    [sql|
      SELECT EXTRACT(ISODOW FROM recorded_at AT TIME ZONE 'America/Los_Angeles')::int4 AS weekday,
             EXTRACT(HOUR FROM recorded_at AT TIME ZONE 'America/Los_Angeles')::int4 AS hour,
             AVG(listener_count)::float8
      FROM listener_snapshots
      WHERE recorded_at >= #{start} AND recorded_at < #{end}
      GROUP BY weekday, hour
      ORDER BY weekday, hour
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
