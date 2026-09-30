{-# LANGUAGE PackageImports #-}

-- | Tests for the break endpoint.
--
-- Liquidsoap asks at @:28@ and @:58@ and cuts whatever is on air when the
-- answer is not empty. So an empty answer must mean "play on". The endpoint
-- reads the stored daily plan, so a repeated request returns the same tracks.
--
-- Every case uses an empty schedule, so each top of the hour is an automation
-- break and no half hour is a break. The planner tests cover what goes into a
-- show break.
module API.Playout.Break.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Break.Get.Handler (action, nextBoundary)
import API.Playout.Types (PlayoutTrack (..))
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
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
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "API.Playout.Break.Get.Handler.nextBoundary" $ do
    it "rounds :28 up to the half hour" boundaryRoundsUpToHalf
    it "rounds :58 up to the hour" boundaryRoundsUpToHour
    it "leaves an instant already on a boundary alone" boundaryIdempotent
    it "moves a fractional second up, never back" boundaryRoundsFractionUp

  withTestDB $
    describe "API.Playout.Break.Get.Handler.action" $ do
      it "returns nothing when the boundary has no break" handlerNoBreakDue
      it "opens the break with a station ID, then PSAs" handlerStationIdFirst
      it "returns the station ID alone when nothing is eligible" handlerStationIdOnly
      it "leaves out an item whose dates have passed" handlerSkipsExpired
      it "airs an item on its final day in the 23:58 break" handlerAirsOnFinalDay
      it "holds back an item that starts the next day" handlerSkipsNotYetStarted
      it "returns the same tracks for a repeated request" handlerIsIdempotent
      it "sends the break item id with each PSA" handlerSendsBreakItemId

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

-- | The result must carry whole seconds, because the plan is keyed by boundary
-- and every stored boundary is a whole half-hour.
--
-- Rounding the fraction down instead would put the boundary behind the caller,
-- so a quarter second past midnight belongs to the 00:30 break.
boundaryRoundsFractionUp :: IO ()
boundaryRoundsFractionUp =
  nextBoundary (UTCTime testDay 0.25) `shouldBe` UTCTime testDay (secondsToDiffTime 1800)

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

-- | 23:58 Pacific, so the next boundary is 00:00 on the following date.
--
-- This is the one break each day that airs on one Pacific date and ends on the
-- next. It belongs to the day it airs on.
atElevenFiftyEight :: UTCTime
atElevenFiftyEight = pacificAt (TimeOfDay 23 58 0)

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

-- | A PSA that runs from January 1.
mkPsaInsert ::
  -- | Title
  Text ->
  -- | Duration in seconds
  Int64 ->
  -- | Last air date
  Maybe Day ->
  User.Id ->
  BreakItems.Insert
mkPsaInsert title secs endsOn creatorId =
  BreakItems.Insert
    { biiTitle = title,
      biiCategory = BreakItems.Psa,
      biiUnderwriterId = Nothing,
      biiSpotsPerMonth = Nothing,
      biiAudioFilePath = "audio/break-items/2025/01/06/" <> title <> ".mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 1024,
      biiDurationSeconds = secs,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = endsOn,
      biiCreatorId = creatorId
    }

addPsa :: Text -> Int64 -> Maybe Day -> User.Id -> TRX.Transaction BreakItems.Id
addPsa title secs endsOn creatorId =
  unwrapInsert $ BreakItems.insertBreakItem (mkPsaInsert title secs endsOn creatorId)

-- | 'addPsa' with an explicit first air date.
addPsaFrom :: Day -> Text -> Int64 -> User.Id -> TRX.Transaction ()
addPsaFrom startsOn title secs creatorId = do
  let base = mkPsaInsert title secs Nothing creatorId
  _ <- unwrapInsert $ BreakItems.insertBreakItem base {BreakItems.biiStartsOn = startsOn}
  pure ()

-- | Insert the user, run the caller's fixture, and fail loudly on a DB error.
runSetup ::
  UserMetadata.UserWithMetadataInsert ->
  (User.Id -> TRX.Transaction a) ->
  AppM a
runSetup userInsert fixture = do
  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    fixture userId
  case dbResult of
    Left err -> error $ "Setup failed: " <> show err
    Right a -> pure a

--------------------------------------------------------------------------------
-- action

-- | Nothing is scheduled and the boundary is a half hour, so no break is due.
handlerNoBreakDue :: TestDBConfig -> IO ()
handlerNoBreakDue cfg = do
  userInsert <- mkUserInsert "break-none" UserMetadata.Staff
  bracketAppM cfg $ do
    _ <- runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsa "psa" 30 Nothing userId
    tracks <- action atTwentyEight
    liftIO $ tracks `shouldSatisfy` null

handlerStationIdFirst :: TestDBConfig -> IO ()
handlerStationIdFirst cfg = do
  userInsert <- mkUserInsert "break-order" UserMetadata.Staff
  bracketAppM cfg $ do
    _ <- runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsa "psa-a" 30 Nothing userId
    tracks <- action atFiftyEight
    liftIO $ do
      map ptSourceType tracks `shouldBe` ["station_id", "psa"]
      map ptBreakItemId (take 1 tracks) `shouldBe` [Nothing]

-- | A break always opens with a station ID, even with nothing to follow it.
handlerStationIdOnly :: TestDBConfig -> IO ()
handlerStationIdOnly cfg = do
  userInsert <- mkUserInsert "break-sid-only" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      pure ()
    tracks <- action atFiftyEight
    liftIO $ map ptSourceType tracks `shouldBe` ["station_id"]

handlerSkipsExpired :: TestDBConfig -> IO ()
handlerSkipsExpired cfg = do
  userInsert <- mkUserInsert "break-expired" UserMetadata.Staff
  bracketAppM cfg $ do
    _ <- runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsa "expired" 30 (Just (fromGregorian 2025 1 5)) userId
    tracks <- action atFiftyEight
    liftIO $ map ptSourceType tracks `shouldBe` ["station_id"]

-- | The 23:58 break airs on 'testDay' but ends at 00:00 the next date.
--
-- An item that runs through 'testDay' belongs in this break. Reading the date
-- off the boundary would drop it from the last break of its own final day.
handlerAirsOnFinalDay :: TestDBConfig -> IO ()
handlerAirsOnFinalDay cfg = do
  userInsert <- mkUserInsert "break-final-day" UserMetadata.Staff
  bracketAppM cfg $ do
    _ <- runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsa "final-day" 30 (Just testDay) userId
    tracks <- action atElevenFiftyEight
    liftIO $ map ptSourceType tracks `shouldBe` ["station_id", "psa"]

-- | The other side of the same boundary.
--
-- An item whose run starts the next date must not air two minutes early.
handlerSkipsNotYetStarted :: TestDBConfig -> IO ()
handlerSkipsNotYetStarted cfg = do
  userInsert <- mkUserInsert "break-not-started" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsaFrom (succ testDay) "tomorrow" 30 userId
    tracks <- action atElevenFiftyEight
    liftIO $ map ptSourceType tracks `shouldBe` ["station_id"]

-- | A retry for the same break must get the same tracks, in the same order.
handlerIsIdempotent :: TestDBConfig -> IO ()
handlerIsIdempotent cfg = do
  userInsert <- mkUserInsert "break-idempotent" UserMetadata.Staff
  bracketAppM cfg $ do
    runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      mapM_ (\t -> addPsa t 30 Nothing userId) ["psa-a", "psa-b", "psa-c"]
    first <- action atFiftyEight
    second <- action atFiftyEight
    liftIO $ do
      first `shouldBe` second
      length first `shouldSatisfy` (> 1)

-- | Liquidsoap reports the id back through @/played@, so every PSA carries it.
handlerSendsBreakItemId :: TestDBConfig -> IO ()
handlerSendsBreakItemId cfg = do
  userInsert <- mkUserInsert "break-item-id" UserMetadata.Staff
  bracketAppM cfg $ do
    psaId <- runSetup userInsert $ \userId -> do
      _ <- insertTestStationId (mkStationIdInsert userId)
      addPsa "psa-a" 30 Nothing userId
    tracks <- action atFiftyEight
    liftIO $
      mapMaybe ptBreakItemId tracks `shouldBe` [BreakItems.unId psaId]
