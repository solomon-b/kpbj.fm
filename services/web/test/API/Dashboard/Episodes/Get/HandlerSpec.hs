{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Handler (EpisodeListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time (DayOfWeek (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestEpisode, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
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
        it "host of the show gets the show selected" test_hostOfShowGetsShowSelected
        it "host who does not host the show gets no selected show" test_unrelatedHostGetsNoSelectedShow

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
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (_userModel, userMetaModel) <- setupUserModels userInsert
        (_, _templateId) <- insertTestShowWithSchedule showInsert scheduleInsert
        pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel "empty-episode-show" Nothing

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
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
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
                  eiScheduleTemplateId = Just templateId,
                  eiAirDate = Just (read "2020-01-01"),
                  eiCreatedBy = userModel.mId
                }
        episodeId <- insertTestEpisode episodeInsert
        episodeModel <- TRX.statement () (Episodes.getEpisodeById episodeId) >>= maybe (error "episode not found after insert") pure
        pure (episodeModel, userMetaModel)
    (episodeModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel "episode-insert-show" Nothing

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
        (_userModel, userMetaModel) <- setupUserModels userInsert
        pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel "no-such-show-slug" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        elvSelectedShow vd `shouldBe` Nothing
        elvEpisodes vd `shouldBe` []

-- | A host who hosts the show gets that show as the selected show.
test_hostOfShowGetsShowSelected :: TestDBConfig -> IO ()
test_hostOfShowGetsShowSelected cfg = do
  userInsert <- mkUserInsert "ep-list-own-host" UserMetadata.Host
  let showInsert = Shows.Insert "Episode List Own Show" "ep-list-own-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        addTestShowHost showId userModel.mId
        pure (userMetaModel, showId)
    (userMetaModel, showId) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel "ep-list-own-show" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> fmap Shows.id (elvSelectedShow vd) `shouldBe` Just showId

-- | A host who hosts a different show gets no selected show for this slug.
--
-- The show exists, but the host does not host it. The page renders with no
-- show and no episodes, the same as for an unknown slug.
test_unrelatedHostGetsNoSelectedShow :: TestDBConfig -> IO ()
test_unrelatedHostGetsNoSelectedShow cfg = do
  userInsert <- mkUserInsert "ep-list-other-host" UserMetadata.Host
  let ownShowInsert = Shows.Insert "Episode List Host Show" "ep-list-host-show" Nothing Nothing Shows.Active
      otherShowInsert = Shows.Insert "Episode List Other Show" "ep-list-other-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        (ownShowId, _) <- insertTestShowWithSchedule ownShowInsert defaultScheduleInsert
        addTestShowHost ownShowId userModel.mId
        _ <- insertTestShowWithSchedule otherShowInsert defaultScheduleInsert
        pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel "ep-list-other-show" Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        elvSelectedShow vd `shouldBe` Nothing
        elvEpisodes vd `shouldBe` []
