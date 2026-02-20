{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Shows.Slug.Get.Handler (ShowViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Shows.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent slug" test_notFoundForMissingShow
        it "returns show detail for a valid slug" test_returnsShowDetail
        it "includes episodes for a show with episodes" test_includesEpisodes

--------------------------------------------------------------------------------

-- | A nonexistent slug returns @Left (NotFound _)@.
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = bracketAppM cfg $ do
  result <- runExceptT $ action (Slug "nonexistent-show-slug") Nothing

  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Inserting a show and fetching by its slug returns the correct show model.
--
-- Verifies that the returned show model has the same slug as the one inserted.
test_returnsShowDetail :: TestDBConfig -> IO ()
test_returnsShowDetail cfg = do
  userInsert <- mkUserInsert "show-detail" UserMetadata.Host
  let targetSlug = Slug "show-detail-test"
      showInsert = Shows.Insert "Show Detail Test" targetSlug Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        addTestShowHost showId userId

    expectSetupRight dbResult

    result <- runExceptT $ action targetSlug Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> Shows.slug vd.svdShowModel `shouldBe` targetSlug

-- | A show with an episode scheduled in the past has a non-empty episodes list.
--
-- @getPublishedEpisodesForShow@ filters by @scheduledAt <= now@, so the episode
-- must be scheduled before the current time to appear in published results.
test_includesEpisodes :: TestDBConfig -> IO ()
test_includesEpisodes cfg = do
  userInsert <- mkUserInsert "show-episodes" UserMetadata.Host
  let targetSlug = Slug "show-with-episodes"
      showInsert = Shows.Insert "Show With Episodes" targetSlug Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        addTestShowHost showId userId
        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Just "Test episode description",
                  eiAudioFilePath = Nothing,
                  eiAudioFileSize = Nothing,
                  eiAudioMimeType = Nothing,
                  eiDurationSeconds = Nothing,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = templateId,
                  eiScheduledAt = read "2020-01-01 10:00:00 UTC",
                  eiCreatedBy = userId
                }
        insertTestEpisode episodeInsert

    _ <- expectSetupRight dbResult

    result <- runExceptT $ action targetSlug Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length vd.svdEpisodes `shouldBe` 1
