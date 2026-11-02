{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the now endpoint.
--
-- Liquidsoap starts an episode at the offset that this endpoint returns. A wrong
-- offset airs the wrong part of a show, or runs the schedule late.
module API.Playout.Now.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Now.Get.Handler (actionAt, offsetSeconds)
import API.Playout.Types (NowPlayingResponse (..), mkPlayoutMetadata)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Int (Int64)
import Data.Time
  ( Day,
    DayOfWeek (..),
    LocalTime (..),
    TimeOfDay (..),
    UTCTime,
    fromGregorian,
  )
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "API.Playout.Now.Get.Handler.offsetSeconds" $ do
    it "counts the seconds since the window opened" offsetCountsSeconds
    it "is 0 before the window opens" offsetBeforeWindow
    it "is 0 when the window opens" offsetAtWindowStart
    it "drops a fraction of a second" offsetDropsFraction

  describe "API.Playout.Types.NowPlayingResponse" $ do
    it "sends the offset as offset_seconds" jsonHasOffset

  withTestDB $
    describe "API.Playout.Now.Get.Handler.actionAt" $ do
      it "returns the offset into the slot" handlerMidSlot
      it "returns 0 for the poll before the slot opens" handlerEarlyPoll
      it "returns the offset into the replay" handlerReplay
      it "returns nothing outside the slot" handlerOutsideSlot

--------------------------------------------------------------------------------

-- | A test date: Monday, January 6, 2025.
testDay :: Day
testDay = fromGregorian 2025 1 6

pacificAt :: TimeOfDay -> UTCTime
pacificAt = pacificToUtc . LocalTime testDay

--------------------------------------------------------------------------------
-- offsetSeconds

offsetCountsSeconds :: IO ()
offsetCountsSeconds =
  offsetSeconds (pacificAt (TimeOfDay 10 20 5)) (pacificAt (TimeOfDay 10 0 0)) `shouldBe` 1205

offsetBeforeWindow :: IO ()
offsetBeforeWindow =
  offsetSeconds (pacificAt (TimeOfDay 9 59 55)) (pacificAt (TimeOfDay 10 0 0)) `shouldBe` 0

offsetAtWindowStart :: IO ()
offsetAtWindowStart =
  offsetSeconds (pacificAt (TimeOfDay 10 0 0)) (pacificAt (TimeOfDay 10 0 0)) `shouldBe` 0

offsetDropsFraction :: IO ()
offsetDropsFraction =
  offsetSeconds (pacificAt (TimeOfDay 10 0 1.9)) (pacificAt (TimeOfDay 10 0 0)) `shouldBe` 1

--------------------------------------------------------------------------------
-- NowPlayingResponse

jsonHasOffset :: IO ()
jsonHasOffset =
  toJSON (NowPlaying "https://example.test/a.mp3" 1205 (mkPlayoutMetadata "Show" "KPBJ"))
    `shouldBe` expected
  where
    expected :: Value
    expected =
      object
        [ "url" .= ("https://example.test/a.mp3" :: String),
          "offset_seconds" .= (1205 :: Int64),
          "title" .= ("Show" :: String),
          "artist" .= ("KPBJ" :: String)
        ]

--------------------------------------------------------------------------------
-- actionAt

-- | Insert an episode that airs from 10:00 to 12:00 on the test date, with a
-- replay at 18:00.
setupEpisode :: UserMetadata.UserWithMetadataInsert -> TRX.Transaction ()
setupEpisode userInsert = do
  (userModel, _) <- setupUserModels userInsert
  (showId, templateId) <-
    insertTestShowWithSchedule
      (Shows.Insert "Now Show" "now-show" Nothing Nothing Shows.Active)
      ShowSchedule.ScheduleTemplateInsert
        { stiShowId = Shows.Id 0,
          stiDayOfWeek = Monday,
          stiWeeksOfMonth = [1, 2, 3, 4, 5],
          stiStartTime = TimeOfDay 10 0 0,
          stiEndTime = TimeOfDay 12 0 0,
          stiTimezone = "America/Los_Angeles",
          stiReplayStartTime = Just (TimeOfDay 18 0 0)
        }
  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = testDay,
            viEffectiveUntil = Nothing
          }
  _ <-
    TRX.statement () $
      Episodes.insertEpisode
        Episodes.Insert
          { eiId = showId,
            eiDescription = Nothing,
            eiAudioFilePath = Just "audio/episodes/now-show.mp3",
            eiAudioFileSize = Nothing,
            eiAudioMimeType = Nothing,
            eiDurationSeconds = Nothing,
            eiArtworkUrl = Nothing,
            eiScheduleTemplateId = Just templateId,
            eiAirDate = Just testDay,
            eiCreatedBy = userModel.mId
          }
  pure ()

-- | Run 'actionAt' at the given instant, with the episode in the database.
responseAt :: TestDBConfig -> UTCTime -> IO (Maybe Int64)
responseAt cfg now = do
  userInsert <- mkUserInsert "now-host" UserMetadata.Host
  bracketAppM cfg $ do
    result <- runDB (TRX.transaction TRX.ReadCommitted TRX.Write (setupEpisode userInsert))
    expectSetupRight result
    offsetOf <$> actionAt now

offsetOf :: NowPlayingResponse -> Maybe Int64
offsetOf (NowPlaying _ offset _) = Just offset
offsetOf NothingPlaying = Nothing

handlerMidSlot :: TestDBConfig -> IO ()
handlerMidSlot cfg = do
  offset <- responseAt cfg (pacificAt (TimeOfDay 10 20 5))
  offset `shouldBe` Just 1205

handlerEarlyPoll :: TestDBConfig -> IO ()
handlerEarlyPoll cfg = do
  offset <- responseAt cfg (pacificAt (TimeOfDay 9 59 55))
  offset `shouldBe` Just 0

handlerReplay :: TestDBConfig -> IO ()
handlerReplay cfg = do
  offset <- responseAt cfg (pacificAt (TimeOfDay 18 10 0))
  offset `shouldBe` Just 600

handlerOutsideSlot :: TestDBConfig -> IO ()
handlerOutsideSlot cfg = do
  offset <- responseAt cfg (pacificAt (TimeOfDay 13 0 0))
  offset `shouldBe` Nothing
