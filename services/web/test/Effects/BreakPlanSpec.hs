{-# LANGUAGE PackageImports #-}

-- | Tests for building, reading, and rebuilding the stored daily break plan.
--
-- Every case uses an empty schedule, so the day has one automation break at
-- each top of the hour: 24 breaks, each opening with a station ID.
module Effects.BreakPlanSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    addDays,
    diffUTCTime,
    fromGregorian,
    getCurrentTime,
  )
import Domain.Types.Timezone (pacificToUtc)
import Effects.BreakPlan
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestStationId, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.BreakPlan" $ do
      it "builds a plan once, and a second call changes nothing" buildsOnce
      it "records a day that has nothing to plan" recordsEmptyDay
      it "leaves out a deleted break item" skipsDeleted
      it "builds within one second" buildIsFast
      it "keeps past entries when it rebuilds" rebuildKeepsPast
      it "forecasts tomorrow as tomorrow's plan" forecastMatchesPlan

--------------------------------------------------------------------------------
-- Fixtures

-- | Monday, January 6, 2025. Clear of both daylight saving changes.
day :: Day
day = fromGregorian 2025 1 6

-- | Insert a staff user, one 10 second station ID, and one 30 second PSA.
--
-- Returns the PSA's id.
seed :: AppM BreakItems.Id
seed = do
  userInsert <- liftIO $ mkUserInsert "break-plan" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    _ <- insertTestStationId (stationIdInsert userId)
    unwrapInsert (BreakItems.insertBreakItem (psaInsert userId))
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right psaId -> pure psaId

stationIdInsert :: User.Id -> StationIds.Insert
stationIdInsert creatorId =
  StationIds.Insert
    { siiTitle = "Station ID",
      siiAudioFilePath = "audio/station-ids/2025/01/06/id_2025-01-06_abc.mp3",
      siiMimeType = "audio/mpeg",
      siiFileSize = 512,
      siiDurationSeconds = Just 10,
      siiCreatorId = creatorId
    }

psaInsert :: User.Id -> BreakItems.Insert
psaInsert creatorId =
  BreakItems.Insert
    { biiTitle = "Library Hours",
      biiCategory = BreakItems.Psa,
      biiUnderwriterId = Nothing,
      biiSpotsPerMonth = Nothing,
      biiAudioFilePath = "audio/break-items/2025/01/01/library_2025-01-01_def.mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 1024,
      biiDurationSeconds = 30,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = Nothing,
      biiCreatorId = creatorId
    }

-- | Read the day's tracks, failing the test on a database error.
dayTracks :: AppM [BreakPlans.PlannedTrack]
dayTracks =
  tracksForDay day >>= \case
    Left err -> error ("Read failed: " <> show err)
    Right tracks -> pure tracks

-- | Build the plan, failing the test on a database error.
build :: AppM ()
build =
  ensurePlan day >>= \case
    Left err -> error ("Build failed: " <> show err)
    Right () -> pure ()

stationIdCount :: [BreakPlans.PlannedTrack] -> Int
stationIdCount tracks = length [() | t <- tracks, t.plSourceType == "station_id"]

--------------------------------------------------------------------------------
-- Cases

buildsOnce :: TestDBConfig -> IO ()
buildsOnce cfg =
  bracketAppM cfg $ do
    _ <- seed
    build
    first <- dayTracks
    build
    second <- dayTracks
    liftIO $ do
      first `shouldBe` second
      stationIdCount first `shouldBe` 24

-- | With no station IDs and no PSAs, the plan has no entries, but the day row
-- still records that the plan exists.
recordsEmptyDay :: TestDBConfig -> IO ()
recordsEmptyDay cfg =
  bracketAppM cfg $ do
    build
    tracks <- dayTracks
    again <- execQuery (BreakPlans.insertPlanDay day)
    liftIO $ do
      tracks `shouldBe` []
      again `shouldBe` Right False

skipsDeleted :: TestDBConfig -> IO ()
skipsDeleted cfg =
  bracketAppM cfg $ do
    psaId <- seed
    build
    beforeDelete <- dayTracks
    _ <- execQuery (BreakItems.softDeleteBreakItem psaId)
    afterDelete <- dayTracks
    let hasPsa = any (\t -> t.plBreakItemId == Just (BreakItems.unId psaId))
    liftIO $ do
      hasPsa beforeDelete `shouldBe` True
      hasPsa afterDelete `shouldBe` False

-- | Liquidsoap allows 5 seconds for the first break request of the day.
buildIsFast :: TestDBConfig -> IO ()
buildIsFast cfg =
  bracketAppM cfg $ do
    _ <- seed
    start <- liftIO getCurrentTime
    build
    end <- liftIO getCurrentTime
    liftIO $ diffUTCTime end start `shouldSatisfy` (< 1)

-- | With nothing aired, the forecast for tomorrow starts from the same state
-- as tomorrow's real build, so the two agree track for track.
forecastMatchesPlan :: TestDBConfig -> IO ()
forecastMatchesPlan cfg =
  bracketAppM cfg $ do
    _ <- seed
    let noon = pacificToUtc (LocalTime day (TimeOfDay 12 0 0))
        tomorrow = addDays 1 day
    forecast <-
      forecastDay noon tomorrow >>= \case
        Left err -> error ("Forecast failed: " <> show err)
        Right tracks -> pure tracks
    built <- ensurePlan tomorrow
    planned <-
      tracksForDay tomorrow >>= \case
        Left err -> error ("Read failed: " <> show err)
        Right tracks -> pure tracks
    liftIO $ do
      built `shouldBe` Right ()
      stationIdCount forecast `shouldBe` 24
      forecast `shouldBe` planned

rebuildKeepsPast :: TestDBConfig -> IO ()
rebuildKeepsPast cfg =
  bracketAppM cfg $ do
    _ <- seed
    build
    original <- dayTracks
    let noon = pacificToUtc (LocalTime day (TimeOfDay 12 0 0))
        past = filter (\t -> t.plBoundary <= noon)
    rebuilt <- rebuildToday noon
    rebuiltTracks <- dayTracks
    liftIO $ do
      rebuilt `shouldBe` Right ()
      past rebuiltTracks `shouldBe` past original
      stationIdCount rebuiltTracks `shouldBe` (24 :: Int)
      length rebuiltTracks `shouldSatisfy` (>= length (past original))
