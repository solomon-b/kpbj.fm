{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @ga_snapshots@.
--
-- Stores top-N session counts for referrers, countries, and cities pulled
-- from the Google Analytics Data API. Populated by the @ga-poller@ systemd
-- job. Each row represents the metric value (sessions) for one dimension
-- value within a closed-open time window @[window_start, window_end)@.
module Effects.Database.Tables.GaSnapshots
  ( -- * Types
    DimensionType (..),
    dimensionTypeText,
    Insert (..),

    -- * Result rows
    TopDimensionRow (..),

    -- * Queries
    insertSnapshot,
    getTopReferrers,
    getTopCountries,
    getTopCities,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------
-- Types

-- | Which Google Analytics dimension a snapshot row describes.
data DimensionType
  = -- | Traffic source (e.g. "google", "twitter.com", "(direct)").
    Source
  | -- | Country name as reported by GA.
    Country
  | -- | City name as reported by GA.
    City
  deriving stock (Eq, Show)

-- | Database text encoding for 'DimensionType'. Matches the CHECK constraint
-- in the @ga_snapshots@ migration.
dimensionTypeText :: DimensionType -> Text
dimensionTypeText = \case
  Source -> "source"
  Country -> "country"
  City -> "city"

-- | Row to insert into @ga_snapshots@.
data Insert = Insert
  { dimensionType :: DimensionType,
    dimensionValue :: Text,
    metricValue :: Int64,
    windowStart :: UTCTime,
    windowEnd :: UTCTime
  }
  deriving stock (Eq, Show)

-- | Result row for top-N dimension queries.
data TopDimensionRow = TopDimensionRow
  { value :: Text,
    sessions :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance TopDimensionRow)

--------------------------------------------------------------------------------
-- Insert

-- | Insert a single snapshot row.
--
-- The poller runs this once per @(dimension, value)@ pair returned by GA.
insertSnapshot :: Insert -> Hasql.Statement () ()
insertSnapshot row =
  interp
    False
    [sql|
      INSERT INTO ga_snapshots
        (dimension_type, dimension_value, metric_value, window_start, window_end)
      VALUES
        ( #{dimensionTypeText (dimensionType row)}
        , #{dimensionValue row}
        , #{metricValue row}
        , #{windowStart row}
        , #{windowEnd row}
        )
    |]

--------------------------------------------------------------------------------
-- Read queries

-- | Top N referrer sources by total sessions in the given range.
getTopReferrers :: UTCTime -> UTCTime -> Int32 -> Hasql.Statement () [TopDimensionRow]
getTopReferrers = topByDimension Source

-- | Top N countries by total sessions in the given range.
getTopCountries :: UTCTime -> UTCTime -> Int32 -> Hasql.Statement () [TopDimensionRow]
getTopCountries = topByDimension Country

-- | Top N cities by total sessions in the given range.
getTopCities :: UTCTime -> UTCTime -> Int32 -> Hasql.Statement () [TopDimensionRow]
getTopCities = topByDimension City

-- | Aggregate snapshots whose window falls within @[start, end)@, grouped
-- by @dimension_value@, summing @metric_value@. Returns the top @n@ rows.
topByDimension :: DimensionType -> UTCTime -> UTCTime -> Int32 -> Hasql.Statement () [TopDimensionRow]
topByDimension dim start end n =
  interp
    False
    [sql|
      SELECT dimension_value, SUM(metric_value)::bigint
      FROM ga_snapshots
      WHERE dimension_type = #{dimensionTypeText dim}
        AND window_start >= #{start}
        AND window_end   <= #{end}
      GROUP BY dimension_value
      ORDER BY SUM(metric_value) DESC
      LIMIT #{n}
    |]
