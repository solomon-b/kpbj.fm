-- | Build and read the stored daily break plan.
--
-- Today's plan is built the first time a break asks for it, or when staff open
-- today's plan view. A future day is only forecast, in memory. The build runs inside Liquidsoap's first break request of
-- the day, which times out after 5 seconds. So it gathers its inputs in a few
-- queries, and the planner itself is a pure function.
module Effects.BreakPlan
  ( assumedStationIdSeconds,
    dayBoundaries,
    ensurePlan,
    forecastDay,
    tracksForBoundary,
    tracksForDay,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Control.Monad (forM_, when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (Day, UTCTime, addDays, addUTCTime)
import Domain.BreakPlanner
import Domain.Types.Timezone (pacificDay, startOfPacificDay)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.ShowSchedule (BreakKind)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.StationIds qualified as StationIds
import Hasql.Pool (UsageError)
import Hasql.Transaction qualified as HT

--------------------------------------------------------------------------------

-- | The length assumed for a station ID with no recorded length.
--
-- Station IDs uploaded before durations were recorded carry none. Guessing a
-- length keeps them usable until their durations are filled in.
assumedStationIdSeconds :: Int64
assumedStationIdSeconds = 15

-- | The first and last boundary whose break belongs to this Pacific day.
--
-- A break belongs to the day of its start, 120 seconds before its boundary. So
-- the 00:00 boundary belongs to the day before, and the day runs from the 00:30
-- boundary to the next day's 00:00 boundary.
dayBoundaries :: Day -> (UTCTime, UTCTime)
dayBoundaries day =
  (addUTCTime 1800 (startOfPacificDay day), startOfPacificDay (addDays 1 day))

-- | Build the day's plan if it does not exist yet.
--
-- A second call finds the day row and does nothing, so every caller sees the
-- same plan.
ensurePlan :: Day -> AppM (Either UsageError ())
ensurePlan day = execTransaction $ do
  created <- HT.statement () (BreakPlans.insertPlanDay day)
  when created $ do
    let (firstBoundary, lastBoundary) = dayBoundaries day
    breaks <- HT.statement () (ShowSchedule.breakKindsBetween firstBoundary lastBoundary)
    items <- HT.statement () (BreakItems.getActiveOnDay day)
    stationIds <- HT.statement () StationIds.getAllForPlan
    stationIdUsed <- Map.fromList <$> HT.statement () BreakPlans.stationIdLastUsed
    itemUsed <- Map.fromList <$> HT.statement () BreakPlans.breakItemLastUsed

    let input = mkInput day breaks items stationIds stationIdUsed itemUsed

    forM_ (planDay input) $ \e ->
      HT.statement () $ case peRef e of
        StationIdRef sid -> BreakPlans.insertEntry (peBoundary e) (pePosition e) (Just sid) Nothing
        BreakItemRef bid -> BreakPlans.insertEntry (peBoundary e) (pePosition e) Nothing (Just bid)

-- | The planner input for one day.
mkInput ::
  Day ->
  -- | The day's breaks to plan, in time order
  [(UTCTime, BreakKind)] ->
  -- | Break items active on the day
  [BreakItems.Model] ->
  [StationIds.Model] ->
  -- | Last planned boundary per station ID
  Map StationIds.Id UTCTime ->
  -- | Last planned boundary per break item
  Map BreakItems.Id UTCTime ->
  PlanInput
mkInput day breaks items stationIds stationIdUsed itemUsed =
  PlanInput
    { piDay = day,
      piBreaks = [PlanBreak b k | (b, k) <- breaks],
      piStationIds =
        [ StationIdCandidate
            s.simId
            (fromMaybe assumedStationIdSeconds s.simDurationSeconds)
            (Map.lookup s.simId stationIdUsed)
        | s <- stationIds
        ],
      piPsas =
        [ PsaCandidate i.bimId i.bimDurationSeconds (Map.lookup i.bimId itemUsed)
        | i <- items,
          i.bimCategory == BreakItems.Psa
        ],
      piCreatives = mapMaybe toCreative items
    }
  where
    toCreative i = do
      uw <- i.bimUnderwriterId
      pure
        Creative
          { crId = i.bimId,
            crUnderwriterId = uw,
            crSeconds = i.bimDurationSeconds,
            crLastUsed = Map.lookup i.bimId itemUsed
          }

-- | Forecast the tracks of a future day, without storing anything.
--
-- Each day's rotation depends on what the days before it played. So this plans
-- every day from tomorrow to the target in memory, starting from the stored
-- plans, and assumes each planned track airs.
--
-- A new item, a deleted item, or a schedule change moves the forecast. The
-- real plan for a day is built on that day.
forecastDay :: UTCTime -> Day -> AppM (Either UsageError [BreakPlans.PlannedTrack])
forecastDay now target
  | target <= today = pure (Right [])
  | otherwise = runExceptT $ do
      ExceptT (ensurePlan today)
      kinds <- ExceptT $ execQuery (ShowSchedule.breakKindsBetween rangeStart rangeEnd)
      items <- ExceptT $ execQuery (BreakItems.getActiveInMonth firstDay target)
      stationIds <- ExceptT $ execQuery StationIds.getAllForPlan
      stationIdUsed <- Map.fromList <$> ExceptT (execQuery BreakPlans.stationIdLastUsed)
      itemUsed <- Map.fromList <$> ExceptT (execQuery BreakPlans.breakItemLastUsed)
      let entries = go firstDay stationIdUsed itemUsed
            where
              go d dStationUsed dItemUsed =
                let (b0, b1) = dayBoundaries d
                    dayItems = [i | i <- items, i.bimStartsOn <= d, maybe True (>= d) i.bimEndsOn]
                    dayBreaks = [(b, k) | (b, k) <- kinds, b >= b0, b <= b1]
                    planned = planDay (mkInput d dayBreaks dayItems stationIds dStationUsed dItemUsed)
                 in if d >= target
                      then planned
                      else
                        go
                          (addDays 1 d)
                          (Map.union (Map.fromList [(sid, b) | PlanEntry b _ (StationIdRef sid) <- planned]) dStationUsed)
                          (Map.union (Map.fromList [(bid, b) | PlanEntry b _ (BreakItemRef bid) <- planned]) dItemUsed)
          stationById = Map.fromList [(s.simId, s) | s <- stationIds]
          itemById = Map.fromList [(i.bimId, i) | i <- items]
      pure (mapMaybe (toTrack stationById itemById) entries)
  where
    today = pacificDay now
    firstDay = addDays 1 today
    (rangeStart, _) = dayBoundaries firstDay
    (_, rangeEnd) = dayBoundaries target
    toTrack stationById itemById e = case peRef e of
      StationIdRef sid -> do
        s <- Map.lookup sid stationById
        pure (BreakPlans.PlannedTrack (peBoundary e) (pePosition e) "station_id" s.simTitle s.simAudioFilePath Nothing)
      BreakItemRef bid -> do
        i <- Map.lookup bid itemById
        let sourceType = case i.bimCategory of
              BreakItems.Psa -> "psa"
              BreakItems.Underwriting -> "underwriting"
        pure (BreakPlans.PlannedTrack (peBoundary e) (pePosition e) sourceType i.bimTitle i.bimAudioFilePath (Just (BreakItems.unId bid)))

-- | The planned tracks of one break, in play order.
tracksForBoundary :: UTCTime -> AppM (Either UsageError [BreakPlans.PlannedTrack])
tracksForBoundary boundary = execQuery (BreakPlans.tracksBetween boundary boundary)

-- | The planned tracks of every break of a day, in time and play order.
tracksForDay :: Day -> AppM (Either UsageError [BreakPlans.PlannedTrack])
tracksForDay day = execQuery (uncurry BreakPlans.tracksBetween (dayBoundaries day))
