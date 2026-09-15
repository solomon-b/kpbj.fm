{-# LANGUAGE PackageImports #-}

-- | Tests for the break window endpoint.
--
-- Liquidsoap asks at @:28@ and @:58@ and cuts whatever is on air when the
-- answer is not empty. So an empty answer must mean "play on", and a non-empty
-- one must fit inside the two minutes before the boundary.
module API.Playout.Break.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Break.Get.Handler
  ( assumedStationIdSeconds,
    breakWindowSeconds,
    fillWindow,
    handlerAt,
    nextBoundary,
  )
import API.Playout.Types (PlayoutTrack (..))
import "kpbj-web" App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    UTCTime (..),
    fromGregorian,
    secondsToDiffTime,
  )
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestStationId, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "API.Playout.Break.Get.Handler.nextBoundary" $ do
    it "rounds :28 up to the half hour" boundaryRoundsUpToHalf
    it "rounds :58 up to the hour" boundaryRoundsUpToHour
    it "leaves an instant already on a boundary alone" boundaryIdempotent
    it "moves a fractional second up, never back" boundaryRoundsFractionUp

  describe "API.Playout.Break.Get.Handler.fillWindow" $ do
    it "takes everything when it all fits" fillTakesAll
    it "stops once the budget is spent" fillStopsAtBudget
    it "skips an item that does not fit and keeps going" fillSkipsAndContinues
    it "returns nothing for an empty budget" fillEmptyBudget

  withTestDB $
    describe "API.Playout.Break.Get.Handler.handlerAt" $ do
      it "returns nothing when no break is due" handlerNoBreakDue
      it "opens the window with a station ID" handlerStationIdFirst
      it "returns the station ID alone when nothing is eligible" handlerStationIdOnly
      it "keeps the window inside its budget" handlerRespectsBudget
      it "leaves out an item whose dates have passed" handlerSkipsExpired
      it "advances the rotation between windows" handlerRotates

--------------------------------------------------------------------------------
-- nextBoundary

-- | A test date: Monday, January 6, 2025.
testDay :: Day
testDay = fromGregorian 2025 1 6

pacificAt :: TimeOfDay -> UTCTime
pacificAt tod = pacificToUtc (LocalTime testDay tod)

boundaryRoundsUpToHalf :: IO ()
boundaryRoundsUpToHalf =
  nextBoundary (pacificAt (TimeOfDay 10 28 0)) `shouldBe` pacificAt (TimeOfDay 10 30 0)

boundaryRoundsUpToHour :: IO ()
boundaryRoundsUpToHour =
  nextBoundary (pacificAt (TimeOfDay 10 58 0)) `shouldBe` pacificAt (TimeOfDay 11 0 0)

boundaryIdempotent :: IO ()
boundaryIdempotent =
  nextBoundary (pacificAt (TimeOfDay 11 0 0)) `shouldBe` pacificAt (TimeOfDay 11 0 0)

-- | The result must carry whole seconds, because isBreakDue compares it to a
-- slot end for equality and a slot end is a date plus a TIME.
--
-- Rounding the fraction down instead would put the boundary behind the caller,
-- so a quarter second past midnight belongs to the 00:30 window.
boundaryRoundsFractionUp :: IO ()
boundaryRoundsFractionUp =
  nextBoundary (UTCTime testDay 0.25) `shouldBe` UTCTime testDay (secondsToDiffTime 1800)

--------------------------------------------------------------------------------
-- fillWindow

-- | A break item that is nothing but a length, for the pure cases.
itemOfLength :: Int64 -> BreakItems.Model
itemOfLength secs =
  BreakItems.BreakItem
    { BreakItems.bimId = BreakItems.Id secs,
      BreakItems.bimTitle = "Item " <> Text.pack (show secs),
      BreakItems.bimCategory = BreakItems.Psa,
      BreakItems.bimAudioFilePath = "audio/break-items/test.mp3",
      BreakItems.bimMimeType = "audio/mpeg",
      BreakItems.bimFileSize = 1024,
      BreakItems.bimDurationSeconds = secs,
      BreakItems.bimStartsOn = testDay,
      BreakItems.bimEndsOn = Nothing,
      BreakItems.bimPriority = 0,
      BreakItems.bimLastPlayedAt = Nothing,
      BreakItems.bimCreatorId = User.Id 1,
      BreakItems.bimCreatedAt = UTCTime testDay 0,
      BreakItems.bimUpdatedAt = UTCTime testDay 0,
      BreakItems.bimDeletedAt = Nothing
    }

lengthsOf :: [BreakItems.Model] -> [Int64]
lengthsOf = map BreakItems.bimDurationSeconds

fillTakesAll :: IO ()
fillTakesAll =
  lengthsOf (fillWindow 120 (map itemOfLength [30, 31, 32])) `shouldBe` [30, 31, 32]

fillStopsAtBudget :: IO ()
fillStopsAtBudget =
  lengthsOf (fillWindow 100 (map itemOfLength [60, 30, 25])) `shouldBe` [60, 30]

-- | A long item near the front must not hide the shorter ones behind it.
fillSkipsAndContinues :: IO ()
fillSkipsAndContinues =
  lengthsOf (fillWindow 60 (map itemOfLength [50, 45, 10])) `shouldBe` [50, 10]

fillEmptyBudget :: IO ()
fillEmptyBudget =
  fillWindow 0 (map itemOfLength [30]) `shouldSatisfy` null

--------------------------------------------------------------------------------
-- Fixtures

-- | 10:58 Pacific, so the next boundary is 11:00, the top of an hour.
--
-- With no slot spanning 11:00, a break is due.
atFiftyEight :: UTCTime
atFiftyEight = pacificAt (TimeOfDay 10 58 0)

-- | 10:28 Pacific, so the next boundary is 10:30.
--
-- That is not the top of an hour, so with nothing scheduled no break is due.
atTwentyEight :: UTCTime
atTwentyEight = pacificAt (TimeOfDay 10 28 0)

mkStationIdInsert :: User.Id -> StationIds.Insert
mkStationIdInsert creatorId =
  StationIds.Insert
    { siiTitle = "Test Station ID",
      siiAudioFilePath = "audio/station-ids/2025/01/06/test_2025-01-06_def456.mp3",
      siiMimeType = "audio/mpeg",
      siiFileSize = 512,
      siiDurationSeconds = Just 10,
      siiCreatorId = creatorId
    }

mkBreakItemInsert ::
  -- | Title
  Text ->
  -- | Duration in seconds
  Int64 ->
  -- | Last air date
  Maybe Day ->
  User.Id ->
  BreakItems.Insert
mkBreakItemInsert title secs endsOn creatorId =
  BreakItems.Insert
    { biiTitle = title,
      biiCategory = BreakItems.Advertisement,
      biiAudioFilePath = "audio/break-items/2025/01/06/" <> title <> ".mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 1024,
      biiDurationSeconds = secs,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = endsOn,
      biiPriority = 0,
      biiCreatorId = creatorId
    }

addBreakItem :: Text -> Int64 -> Maybe Day -> User.Id -> TRX.Transaction ()
addBreakItem title secs endsOn creatorId = do
  _ <- unwrapInsert $ BreakItems.insertBreakItem (mkBreakItemInsert title secs endsOn creatorId)
  pure ()

-- | Insert the user, run the caller's fixture, and fail loudly on a DB error.
runSetup ::
  UserMetadata.UserWithMetadataInsert ->
  (User.Id -> TRX.Transaction ()) ->
  AppM ()
runSetup userInsert fixture = do
  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    fixture userId
  liftIO $ case dbResult of
    Left err -> error $ "Setup failed: " <> show err
    Right () -> pure ()

--------------------------------------------------------------------------------
-- handlerAt

-- | Nothing is scheduled and the boundary is a half hour, so no break is due.
handlerNoBreakDue :: TestDBConfig -> IO ()
handlerNoBreakDue cfg = do
  userInsert <- mkUserInsert "break-none" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addBreakItem "spot" 30 Nothing userId
    tracks <- handlerAt atTwentyEight
    liftIO $ tracks `shouldSatisfy` null

handlerStationIdFirst :: TestDBConfig -> IO ()
handlerStationIdFirst cfg = do
  userInsert <- mkUserInsert "break-order" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addBreakItem "spot-a" 30 Nothing userId
    tracks <- handlerAt atFiftyEight
    liftIO $ do
      length tracks `shouldBe` 2
      case tracks of
        (firstTrack : secondTrack : _) -> do
          ptSourceType firstTrack `shouldBe` "station_id"
          ptSourceType secondTrack `shouldBe` "break_item"
        _ -> error "Expected two tracks"

-- | The window always opens with a station ID, even with nothing to follow it.
handlerStationIdOnly :: TestDBConfig -> IO ()
handlerStationIdOnly cfg = do
  userInsert <- mkUserInsert "break-sid-only" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      pure ()
    tracks <- handlerAt atFiftyEight
    liftIO $ do
      length tracks `shouldBe` 1
      case tracks of
        (firstTrack : _) -> ptSourceType firstTrack `shouldBe` "station_id"
        _ -> error "Expected one track"

-- | Four 45 second spots cannot all fit behind a 10 second station ID.
--
-- The budget is 120 less the station ID's 10, so two spots fit and two do not.
handlerRespectsBudget :: TestDBConfig -> IO ()
handlerRespectsBudget cfg = do
  userInsert <- mkUserInsert "break-budget" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      mapM_
        (\n -> addBreakItem ("spot-" <> Text.pack (show (n :: Int))) 45 Nothing userId)
        [1 .. 4]
    tracks <- handlerAt atFiftyEight
    liftIO $ do
      breakWindowSeconds `shouldBe` 120
      assumedStationIdSeconds `shouldBe` 15
      length tracks `shouldBe` 3

handlerSkipsExpired :: TestDBConfig -> IO ()
handlerSkipsExpired cfg = do
  userInsert <- mkUserInsert "break-expired" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addBreakItem "expired" 30 (Just (fromGregorian 2025 1 5)) userId
    tracks <- handlerAt atFiftyEight
    liftIO $ do
      length tracks `shouldBe` 1
      case tracks of
        (firstTrack : _) -> ptSourceType firstTrack `shouldBe` "station_id"
        _ -> error "Expected one track"

-- | The second window must not repeat the first window's choice.
--
-- Two spots of equal priority, and room for only one. The first call stamps
-- last_played_at, which sends the second call to the other spot.
handlerRotates :: TestDBConfig -> IO ()
handlerRotates cfg = do
  userInsert <- mkUserInsert "break-rotate" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addBreakItem "spot-a" 100 Nothing userId
      addBreakItem "spot-b" 100 Nothing userId

    firstRun <- handlerAt atFiftyEight
    secondRun <- handlerAt atFiftyEight

    liftIO $ case (drop 1 firstRun, drop 1 secondRun) of
      ([a], [b]) -> ptTitle a `shouldSatisfy` (/= ptTitle b)
      other -> error $ "Expected one break item in each window, got " <> show other
