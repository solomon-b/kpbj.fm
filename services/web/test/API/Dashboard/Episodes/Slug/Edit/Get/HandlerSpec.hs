{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Handler (EpisodeEditViewData (..), action, fetchEpisodeContext)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time (DayOfWeek (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Episodes.Slug.Edit.Get.Handler" $ do
      describe "fetchEpisodeContext" $ do
        it "returns episode for valid show slug and episode number" test_fetchValidEpisode
        it "returns NotFound for missing episode" test_fetchMissingEpisode
        it "identifies host correctly" test_identifiesHost
        it "admin is always treated as host" test_adminIsHost
      describe "action" $ do
        it "host of the show can open the edit form for an episode of another creator" test_actionAllowsHostOfShow
        it "host who does not host the show and did not create the episode gets NotAuthorized" test_actionRejectsUnrelatedHost

--------------------------------------------------------------------------------

-- | Insert a show, schedule, episode, and user for testing.
setupFixture ::
  Shows.Insert ->
  UserMetadata.UserWithMetadataInsert ->
  TRX.Transaction (Shows.Model, Episodes.Model, User.Model, UserMetadata.Model)
setupFixture showInsert userInsert = do
  (userModel, userMetaModel) <- setupUserModels userInsert

  let scheduleInsert =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
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
            eiAirDate = Just (read "2026-03-01"),
            eiCreatedBy = userModel.mId
          }
  -- Note: 'error' is used here because Transaction does not have MonadIO,
  -- so HUnit assertions are not available in this context.
  episodeId <-
    TRX.statement () (Episodes.insertEpisode episodeInsert)
      >>= maybe (error "setupFixture: episode insert returned Nothing") pure

  showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "setupFixture: show not found") pure
  episodeModel <- TRX.statement () (Episodes.getEpisodeById episodeId) >>= maybe (error "setupFixture: episode not found") pure

  pure (showModel, episodeModel, userModel, userMetaModel)

--------------------------------------------------------------------------------

-- | Successfully fetches episode context for a valid show/episode.
test_fetchValidEpisode :: TestDBConfig -> IO ()
test_fetchValidEpisode cfg = do
  userInsert <- mkUserInsert "valid-ep" UserMetadata.Host
  let showInsert = Shows.Insert "Valid Episode Show" "valid-episode-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB (TRX.transaction TRX.ReadCommitted TRX.Write (setupFixture showInsert userInsert))
    (showModel, episodeModel, userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ fetchEpisodeContext showModel.slug episodeModel.episodeNumber userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right (episode, showResult, _tracks, _isHost) -> do
        Episodes.id episode `shouldBe` Episodes.id episodeModel
        Shows.id showResult `shouldBe` Shows.id showModel

-- | Returns NotFound when episode does not exist.
test_fetchMissingEpisode :: TestDBConfig -> IO ()
test_fetchMissingEpisode cfg = do
  userInsert <- mkUserInsert "missing-ep" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ fetchEpisodeContext "nonexistent-show" 999 userModel userMetaModel

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Non-host user gets isHost = False, host user gets isHost = True.
test_identifiesHost :: TestDBConfig -> IO ()
test_identifiesHost cfg = do
  userInsert <- mkUserInsert "host-check" UserMetadata.User
  let showInsert = Shows.Insert "Host Test Show" "host-test-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB (TRX.transaction TRX.ReadCommitted TRX.Write (setupFixture showInsert userInsert))
    (showModel, episodeModel, userModel, userMetaModel) <- expectSetupRight dbResult

    -- Without being a host, isHost should be False
    result1 <- runExceptT $ fetchEpisodeContext showModel.slug episodeModel.episodeNumber userModel userMetaModel

    -- Add user as host
    _ <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          TRX.statement () (ShowHost.addHostToShow showModel.id userModel.mId)

    -- Now isHost should be True
    result2 <- runExceptT $ fetchEpisodeContext showModel.slug episodeModel.episodeNumber userModel userMetaModel

    liftIO $ do
      case result1 of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
        Right (_, _, _, isHost1) -> isHost1 `shouldBe` False

      case result2 of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
        Right (_, _, _, isHost2) -> isHost2 `shouldBe` True

-- | Admin users are always treated as hosts regardless of show_hosts.
test_adminIsHost :: TestDBConfig -> IO ()
test_adminIsHost cfg = do
  userInsert <- mkUserInsert "admin-host" UserMetadata.Admin
  let showInsert = Shows.Insert "Admin Test Show" "admin-test-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB (TRX.transaction TRX.ReadCommitted TRX.Write (setupFixture showInsert userInsert))
    (showModel, episodeModel, userModel, userMetaModel) <- expectSetupRight dbResult

    -- Admin is not in show_hosts but should still be treated as host
    result <- runExceptT $ fetchEpisodeContext showModel.slug episodeModel.episodeNumber userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right (_, _, _, isHost) -> isHost `shouldBe` True

-- | A host of the show can edit an episode that another user created.
test_actionAllowsHostOfShow :: TestDBConfig -> IO ()
test_actionAllowsHostOfShow cfg = do
  ownerInsert <- mkUserInsert "edit-action-owner" UserMetadata.Admin
  hostInsert <- mkUserInsert "edit-action-host" UserMetadata.Host
  let showInsert = Shows.Insert "Edit Action Host Show" "edit-action-host-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (showModel, episodeModel, _ownerModel, _ownerMeta) <- setupFixture showInsert ownerInsert
          (hostModel, hostMetaModel) <- setupUserModels hostInsert
          TRX.statement () (ShowHost.addHostToShow (Shows.id showModel) hostModel.mId)
          pure (showModel, episodeModel, hostModel, hostMetaModel)
    (showModel, episodeModel, hostModel, hostMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action hostModel hostMetaModel showModel.slug episodeModel.episodeNumber

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> Shows.id (eevShowModel vd) `shouldBe` Shows.id showModel

-- | A host who does not host the show and did not create the episode gets
-- NotAuthorized.
test_actionRejectsUnrelatedHost :: TestDBConfig -> IO ()
test_actionRejectsUnrelatedHost cfg = do
  ownerInsert <- mkUserInsert "edit-action-other-owner" UserMetadata.Admin
  visitorInsert <- mkUserInsert "edit-action-visitor" UserMetadata.Host
  let showInsert = Shows.Insert "Edit Action Other Show" "edit-action-other-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (showModel, episodeModel, _ownerModel, _ownerMeta) <- setupFixture showInsert ownerInsert
          (visitorModel, visitorMetaModel) <- setupUserModels visitorInsert
          pure (showModel, episodeModel, visitorModel, visitorMetaModel)
    (showModel, episodeModel, visitorModel, visitorMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action visitorModel visitorMetaModel showModel.slug episodeModel.episodeNumber

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
