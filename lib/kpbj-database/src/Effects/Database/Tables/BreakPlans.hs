{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definitions and queries for the stored daily break plan.
--
-- The plan for a Pacific day is one row in @break_plan_days@ and one row in
-- @break_plan_entries@ for each track of each break. The break endpoint reads
-- the entries for its boundary and chooses nothing, so a repeated request
-- returns the same tracks.
module Effects.Database.Tables.BreakPlans
  ( -- * Table Definitions
    BreakPlanDay (..),
    breakPlanDaySchema,
    BreakPlanEntry (..),
    breakPlanEntrySchema,

    -- * Result Types
    PlannedTrack (..),

    -- * Queries
    insertPlanDay,
    insertEntry,
    tracksBetween,
    stationIdLastUsed,
    breakItemLastUsed,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (Day, UTCTime)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.StationIds qualified as StationIds
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified

--------------------------------------------------------------------------------
-- Table Definitions

-- | The @break_plan_days@ table. A row means the plan for that day exists.
newtype BreakPlanDay f = BreakPlanDay
  { bpdDay :: Column f Day
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Table schema connecting 'BreakPlanDay' to the database table.
--
-- @created_at@ is left out. Its default fills it, and no query reads it.
breakPlanDaySchema :: TableSchema (BreakPlanDay Name)
breakPlanDaySchema =
  TableSchema
    { name = "break_plan_days",
      columns = BreakPlanDay {bpdDay = "day"}
    }

-- | The @break_plan_entries@ table. One row is one track of one planned break.
--
-- Exactly one of the two references is set.
data BreakPlanEntry f = BreakPlanEntry
  { bpeBoundary :: Column f UTCTime,
    bpePosition :: Column f Int64,
    bpeStationIdId :: Column f (Maybe StationIds.Id),
    bpeBreakItemId :: Column f (Maybe BreakItems.Id)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Table schema connecting 'BreakPlanEntry' to the database table.
breakPlanEntrySchema :: TableSchema (BreakPlanEntry Name)
breakPlanEntrySchema =
  TableSchema
    { name = "break_plan_entries",
      columns =
        BreakPlanEntry
          { bpeBoundary = "boundary",
            bpePosition = "position",
            bpeStationIdId = "station_id_id",
            bpeBreakItemId = "break_item_id"
          }
    }

--------------------------------------------------------------------------------
-- Result Types

-- | One planned track, joined to the row it names.
data PlannedTrack = PlannedTrack
  { plBoundary :: UTCTime,
    plPosition :: Int64,
    -- | @station_id@, @psa@, or @underwriting@.
    plSourceType :: Text,
    plTitle :: Text,
    plAudioFilePath :: Text,
    -- | The break item this track plays. Nothing for a station ID.
    plBreakItemId :: Maybe Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display PlannedTrack where
  displayBuilder t = displayBuilder t.plSourceType <> " - " <> displayBuilder t.plTitle

--------------------------------------------------------------------------------
-- Queries

-- | Record that a day's plan exists. True when this call created the row.
--
-- The primary key lets only one request build a day's plan. A second request
-- gets False, waits for the first to commit, and reads the rows it wrote.
insertPlanDay :: Day -> Hasql.Statement () Bool
insertPlanDay day =
  not . Prelude.null
    <$> run
      ( insert
          Rel8.Insert
            { into = breakPlanDaySchema,
              rows = values [BreakPlanDay {bpdDay = lit day}],
              onConflict = DoNothing,
              returning = Returning bpdDay
            }
      )

-- | Insert one planned track.
insertEntry :: UTCTime -> Int64 -> Maybe StationIds.Id -> Maybe BreakItems.Id -> Hasql.Statement () ()
insertEntry boundary position stationIdId breakItemId =
  run_ $
    insert
      Rel8.Insert
        { into = breakPlanEntrySchema,
          rows =
            values
              [ BreakPlanEntry
                  { bpeBoundary = lit boundary,
                    bpePosition = lit position,
                    bpeStationIdId = lit stationIdId,
                    bpeBreakItemId = lit breakItemId
                  }
              ],
          onConflict = Abort,
          returning = NoReturning
        }

-- | Planned tracks for the boundaries in the inclusive range, in play order.
--
-- A break item that staff deleted is left out. So pulling an announcement takes
-- effect at once, although the plan still names it.
tracksBetween :: UTCTime -> UTCTime -> Hasql.Statement () [PlannedTrack]
tracksBetween fromBoundary toBoundary =
  interp
    False
    [sql|
    SELECT e.boundary, e.position, 'station_id'::TEXT, s.title, s.audio_file_path, NULL::BIGINT
    FROM break_plan_entries e
    JOIN station_ids s ON s.id = e.station_id_id
    WHERE e.boundary BETWEEN #{fromBoundary} AND #{toBoundary}
    UNION ALL
    SELECT e.boundary, e.position, bi.category::TEXT, bi.title, bi.audio_file_path, bi.id
    FROM break_plan_entries e
    JOIN break_items bi ON bi.id = e.break_item_id
    WHERE e.boundary BETWEEN #{fromBoundary} AND #{toBoundary}
      AND bi.deleted_at IS NULL
    ORDER BY 1, 2
  |]

-- | The last planned boundary for each station ID.
stationIdLastUsed :: Hasql.Statement () [(StationIds.Id, UTCTime)]
stationIdLastUsed =
  run $
    select $
      aggregate1 ((,) <$> groupByOn fst <*> maxOn snd) do
        entry <- each breakPlanEntrySchema
        stationIdId <- catNull (bpeStationIdId entry)
        pure (stationIdId, bpeBoundary entry)

-- | The last planned boundary for each break item.
breakItemLastUsed :: Hasql.Statement () [(BreakItems.Id, UTCTime)]
breakItemLastUsed =
  run $
    select $
      aggregate1 ((,) <$> groupByOn fst <*> maxOn snd) do
        entry <- each breakPlanEntrySchema
        breakItemId <- catNull (bpeBreakItemId entry)
        pure (breakItemId, bpeBoundary entry)
