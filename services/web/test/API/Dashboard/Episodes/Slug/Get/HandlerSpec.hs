{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Get.Handler (EpisodeDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
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
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Episodes.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for nonexistent show slug and episode number" test_notFoundForMissingEpisode
        it "returns episode detail for admin with valid episode" test_returnsEpisodeDetailForAdmin
        it "returns NotAuthorized for unassigned regular user" test_notAuthorizedForUnassignedUser

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
            stiDayOfWeek = Nothing,
            stiWeeksOfMonth = Nothing,
            stiStartTime = read "10:00:00",
            stiEndTime = read "11:00:00",
            stiTimezone = "America/Los_Angeles",
            stiAirsTwiceDaily = False
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
            eiScheduleTemplateId = templateId,
            eiScheduledAt = read "2020-01-01 10:00:00 UTC",
            eiCreatedBy = userModel.mId
          }
  episodeId <- insertTestEpisode episodeInsert
  showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "setupFixture: show not found") pure
  episodeModel <- TRX.statement () (Episodes.getEpisodeById episodeId) >>= maybe (error "setupFixture: episode not found") pure
  pure (showModel, episodeModel, userModel, userMetaModel)

--------------------------------------------------------------------------------

-- | Admin user requesting a nonexistent slug/number gets NotFound.
test_notFoundForMissingEpisode :: TestDBConfig -> IO ()
test_notFoundForMissingEpisode cfg = do
  userInsert <- mkUserInsert "ep-detail-missing" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel "no-such-show" (Episodes.EpisodeNumber 999)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin user gets full episode detail for an episode that exists.
test_returnsEpisodeDetailForAdmin :: TestDBConfig -> IO ()
test_returnsEpisodeDetailForAdmin cfg = do
  userInsert <- mkUserInsert "ep-detail-admin" UserMetadata.Admin
  let showInsert = Shows.Insert "Admin Detail Show" "admin-detail-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB (TRX.transaction TRX.ReadCommitted TRX.Write (setupFixture showInsert userInsert))
    (showModel, episodeModel, userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel showModel.slug episodeModel.episodeNumber

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        Episodes.id vd.edvEpisode `shouldBe` Episodes.id episodeModel
        Shows.id vd.edvShowModel `shouldBe` Shows.id showModel

-- | Regular user not assigned to the show cannot view the episode detail.
test_notAuthorizedForUnassignedUser :: TestDBConfig -> IO ()
test_notAuthorizedForUnassignedUser cfg = do
  -- Create the show owner separately (admin) so the episode exists
  ownerInsert <- mkUserInsert "ep-detail-owner" UserMetadata.Admin
  -- The regular user is not assigned to any show
  visitorInsert <- mkUserInsert "ep-detail-visitor" UserMetadata.User
  let showInsert = Shows.Insert "Unassigned Detail Show" "unassigned-detail-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
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
