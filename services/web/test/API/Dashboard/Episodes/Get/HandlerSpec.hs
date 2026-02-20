{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Handler (EpisodeListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Episodes.Get.Handler" $ do
      describe "action" $ do
        it "returns empty episode list for valid show with no episodes" test_emptyDbReturnsNoEpisodes
        it "inserted episode appears in episode list" test_insertedEpisodeAppears
        it "nonexistent show slug returns empty data gracefully" test_nonexistentShowReturnsEmptyList

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user with a valid show slug but no episodes yields an empty list.
test_emptyDbReturnsNoEpisodes :: TestDBConfig -> IO ()
test_emptyDbReturnsNoEpisodes cfg = do
  userInsert <- mkUserInsert "ep-list-empty" UserMetadata.Admin
  let showInsert = Shows.Insert "Empty Episode Show" "empty-episode-show" Nothing Nothing Shows.Active
      scheduleInsert =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Nothing,
            stiWeeksOfMonth = Nothing,
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiAirsTwiceDaily = False
          }

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        (_, _templateId) <- insertTestShowWithSchedule showInsert scheduleInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel "empty-episode-show" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> elvEpisodes vd `shouldBe` []

-- | An episode inserted for a show appears in the list returned by action.
test_insertedEpisodeAppears :: TestDBConfig -> IO ()
test_insertedEpisodeAppears cfg = do
  userInsert <- mkUserInsert "ep-list-insert" UserMetadata.Admin
  let showInsert = Shows.Insert "Episode Insert Show" "episode-insert-show" Nothing Nothing Shows.Active
      scheduleInsert =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Nothing,
            stiWeeksOfMonth = Nothing,
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiAirsTwiceDaily = False
          }

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleInsert
        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Just "Test episode",
                  eiAudioFilePath = Nothing,
                  eiAudioFileSize = Nothing,
                  eiAudioMimeType = Nothing,
                  eiDurationSeconds = Nothing,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = templateId,
                  eiScheduledAt = read "2020-01-01 10:00:00 UTC",
                  eiCreatedBy = userModel.mId
                }
        episodeId <- insertTestEpisode episodeInsert
        episodeModel <- TRX.statement () (Episodes.getEpisodeById episodeId) >>= maybe (error "episode not found after insert") pure
        pure (episodeModel, userModel, userMetaModel)
    (episodeModel, userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel "episode-insert-show" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        elvEpisodes vd `shouldSatisfy` (not . null)
        let episodeIds = map Episodes.id (elvEpisodes vd)
        episodeIds `shouldSatisfy` elem (Episodes.id episodeModel)

-- | A slug that matches no known show returns Nothing for selected show and an
-- empty episode list, rather than throwing NotFound.
test_nonexistentShowReturnsEmptyList :: TestDBConfig -> IO ()
test_nonexistentShowReturnsEmptyList cfg = do
  userInsert <- mkUserInsert "ep-list-missing" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel "no-such-show-slug" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        elvSelectedShow vd `shouldBe` Nothing
        elvEpisodes vd `shouldBe` []
