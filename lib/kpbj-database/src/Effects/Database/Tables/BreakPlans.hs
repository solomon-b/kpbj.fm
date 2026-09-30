{-# LANGUAGE QuasiQuotes #-}

-- | Queries for the stored daily break plan.
--
-- The plan for a Pacific day is one row in @break_plan_days@ and one row in
-- @break_plan_entries@ for each track of each break. The break endpoint reads
-- the entries for its boundary and chooses nothing, so a repeated request
-- returns the same tracks.
module Effects.Database.Tables.BreakPlans
  ( -- * Result Types
    PlannedTrack (..),

    -- * Queries
    insertPlanDay,
    insertEntry,
    tracksBetween,
    plannedBoundariesBetween,
    stationIdLastUsed,
    breakItemLastUsed,
    deleteFutureEntries,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (Day, UTCTime)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.StationIds qualified as StationIds
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql

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
  isJust . listToMaybe . map getOneColumn
    <$> ( interp
            False
            [sql|
            INSERT INTO break_plan_days (day) VALUES (#{day})
            ON CONFLICT DO NOTHING
            RETURNING day
          |] ::
            Hasql.Statement () [OneColumn Day]
        )

-- | Insert one planned track.
insertEntry :: UTCTime -> Int64 -> Maybe StationIds.Id -> Maybe BreakItems.Id -> Hasql.Statement () ()
insertEntry boundary position stationIdId breakItemId =
  interp
    False
    [sql|
    INSERT INTO break_plan_entries (boundary, position, station_id_id, break_item_id)
    VALUES (#{boundary}, #{position}, #{stationIdId}, #{breakItemId})
  |]

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

-- | Boundaries in the inclusive range that already have planned entries.
--
-- After a rebuild, entries for past breaks stay. The rebuild must not plan
-- those boundaries again.
plannedBoundariesBetween :: UTCTime -> UTCTime -> Hasql.Statement () [UTCTime]
plannedBoundariesBetween fromBoundary toBoundary =
  map getOneColumn
    <$> interp
      False
      [sql|
      SELECT DISTINCT boundary
      FROM break_plan_entries
      WHERE boundary BETWEEN #{fromBoundary} AND #{toBoundary}
    |]

-- | The last planned boundary for each station ID.
stationIdLastUsed :: Hasql.Statement () [(StationIds.Id, UTCTime)]
stationIdLastUsed =
  interp
    False
    [sql|
    SELECT station_id_id, MAX(boundary)
    FROM break_plan_entries
    WHERE station_id_id IS NOT NULL
    GROUP BY station_id_id
  |]

-- | The last planned boundary for each break item.
breakItemLastUsed :: Hasql.Statement () [(BreakItems.Id, UTCTime)]
breakItemLastUsed =
  interp
    False
    [sql|
    SELECT break_item_id, MAX(boundary)
    FROM break_plan_entries
    WHERE break_item_id IS NOT NULL
    GROUP BY break_item_id
  |]

-- | Remove a day's plan row and its entries after an instant, for a rebuild.
--
-- Entries for breaks already past stay, so the rotation history is kept.
deleteFutureEntries :: Day -> UTCTime -> UTCTime -> Hasql.Statement () ()
deleteFutureEntries day afterInstant dayEnd =
  interp
    False
    [sql|
    WITH gone AS (
      DELETE FROM break_plan_entries
      WHERE boundary > #{afterInstant} AND boundary <= #{dayEnd}
    )
    DELETE FROM break_plan_days WHERE day = #{day}
  |]
