{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Episode.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Handler (EpisodeViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..), execStatement)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Shows.Slug.Episode.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent show slug and episode number" test_notFoundForMissingEpisode
        it "returns episode detail for a valid show slug and episode number" test_returnsEpisodeDetail
        it "includes the correct show model in the result" test_includesShowModel

--------------------------------------------------------------------------------

-- | Build an episode insert for a given show, schedule template, and user.
mkEpisodeInsert ::
  Shows.Id ->
  ShowSchedule.TemplateId ->
  User.Id ->
  Episodes.Insert
mkEpisodeInsert showId templateId userId =
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

--------------------------------------------------------------------------------

-- | A nonexistent show slug and episode number return @Left (NotFound _)@.
test_notFoundForMissingEpisode :: TestDBConfig -> IO ()
test_notFoundForMissingEpisode cfg = bracketAppM cfg $ do
  result <- runExceptT $ action (Slug "no-such-show") (Episodes.EpisodeNumber 9999)

  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Inserting a show and episode and fetching by slug and episode number
-- returns the correct episode model.
--
-- The episode number is auto-assigned by a PostgreSQL trigger, so we read the
-- episode back after insert to obtain it before calling @action@.
test_returnsEpisodeDetail :: TestDBConfig -> IO ()
test_returnsEpisodeDetail cfg = do
  userInsert <- mkUserInsert "ep-detail" UserMetadata.Host
  let targetSlug = Slug "ep-detail-show"
      showInsert = Shows.Insert "Episode Detail Show" targetSlug Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    -- Insert show, schedule, and episode in a single transaction.
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        insertTestEpisode (mkEpisodeInsert showId templateId userId)

    episodeId <- expectSetupRight dbResult

    -- Read back the episode to obtain its auto-assigned episode number.
    mEpisode <- execStatement (Episodes.getEpisodeById episodeId) >>= expectSetupRight

    episodeModel <- case mEpisode of
      Nothing -> liftIO $ assertFailure "Episode not found after insert"
      Just m -> pure m

    actionResult <- runExceptT $ action targetSlug episodeModel.episodeNumber

    liftIO $ case actionResult of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> Episodes.id vd.evdEpisode `shouldBe` episodeId

-- | The show model returned alongside the episode has the same slug as the
-- show that was inserted.
test_includesShowModel :: TestDBConfig -> IO ()
test_includesShowModel cfg = do
  userInsert <- mkUserInsert "ep-show-model" UserMetadata.Host
  let targetSlug = Slug "ep-show-model-show"
      showInsert = Shows.Insert "Episode Show Model Show" targetSlug Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    -- Insert show, schedule, and episode in a single transaction.
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        insertTestEpisode (mkEpisodeInsert showId templateId userId)

    episodeId <- expectSetupRight dbResult

    -- Read back the episode to obtain its auto-assigned episode number.
    mEpisode <- execStatement (Episodes.getEpisodeById episodeId) >>= expectSetupRight

    episodeModel <- case mEpisode of
      Nothing -> liftIO $ assertFailure "Episode not found after insert"
      Just m -> pure m

    actionResult <- runExceptT $ action targetSlug episodeModel.episodeNumber

    liftIO $ case actionResult of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> Shows.slug vd.evdShowModel `shouldBe` targetSlug
