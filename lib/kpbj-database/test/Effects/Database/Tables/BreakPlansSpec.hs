-- | Tests for the stored break plan queries.
--
-- The timeline places each break track at a fixed instant, so it needs the
-- length of every track. These tests check that 'UUT.tracksBetween' reads that
-- length from the table that holds the audio.
module Effects.Database.Tables.BreakPlansSpec where

--------------------------------------------------------------------------------

import Data.Time (UTCTime (..), fromGregorian)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.BreakPlans qualified as UUT
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestStationId, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.StationIds (stationIdInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BreakPlans" $ do
      describe "tracksBetween" $ do
        runs 10 . it "returns the length of a station ID and of a break item" $
          hedgehog . prop_tracksBetweenReturnsLengths

--------------------------------------------------------------------------------
-- Helpers

-- | A break boundary: 2025-01-06 20:00 UTC, which is noon Pacific.
testBoundary :: UTCTime
testBoundary = UTCTime (fromGregorian 2025 1 6) (20 * 3600)

-- | A 45 second PSA that runs from January 1, 2025 with no end.
psaInsert :: User.Id -> BreakItems.Insert
psaInsert creatorId =
  BreakItems.Insert
    { biiTitle = "Library Hours",
      biiCategory = BreakItems.Psa,
      biiUnderwriterId = Nothing,
      biiShowId = Nothing,
      biiReviewStatus = BreakItems.Approved,
      biiAudioFilePath = "audio/break-items/2025/01/01/library-hours_2025-01-01_abc.mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 720000,
      biiDurationSeconds = 45,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = Nothing,
      biiCreatorId = creatorId
    }

--------------------------------------------------------------------------------
-- tracksBetween

-- | A station ID track carries @station_ids.duration_seconds@, and a break item
-- track carries @break_items.duration_seconds@.
prop_tracksBetweenReturnsLengths :: TestDBConfig -> PropertyT IO ()
prop_tracksBetweenReturnsLengths cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    stationIdTemplate <- forAllT $ stationIdInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let stationIdInsert = stationIdTemplate {StationIds.siiCreatorId = userId}
        stationIdId <- insertTestStationId stationIdInsert
        breakItemId <- unwrapInsert (BreakItems.insertBreakItem (psaInsert userId))

        TRX.statement () (UUT.insertEntry testBoundary 0 (Just stationIdId) Nothing)
        TRX.statement () (UUT.insertEntry testBoundary 1 Nothing (Just breakItemId))

        tracks <- TRX.statement () (UUT.tracksBetween testBoundary testBoundary)

        TRX.condemn
        pure (StationIds.siiDurationSeconds stationIdInsert, tracks)

      assert $ do
        (stationIdSeconds, tracks) <- assertRight result
        let lengthsByPosition = [(UUT.plPosition track, UUT.plSeconds track) | track <- tracks]
        lengthsByPosition === [(0, stationIdSeconds), (1, 45)]
