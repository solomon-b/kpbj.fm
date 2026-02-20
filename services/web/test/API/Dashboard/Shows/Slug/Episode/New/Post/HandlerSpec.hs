{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Episode.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Post.Handler (action)
import API.Dashboard.Shows.Slug.Episode.New.Post.Route (EpisodeUploadForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Episode.New.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for non-existent show" test_notFoundForMissingShow
        it "returns ValidationError for missing scheduled date" test_validationErrorForMissingSchedule
        it "returns ValidationError for missing audio token" test_validationErrorForMissingAudio
        it "staff user creates episode with staged audio" test_staffCreatesEpisode

--------------------------------------------------------------------------------

-- | Minimal valid form; all optional fields are Nothing/empty.
minimalForm :: Text -> Maybe Text -> Maybe Text -> EpisodeUploadForm
minimalForm showId mScheduledDate mAudioToken =
  EpisodeUploadForm
    { eufId = showId,
      eufScheduledDate = mScheduledDate,
      eufDescription = "Test episode description.",
      eufTags = Nothing,
      eufDurationSeconds = Just "3600",
      eufTracksJson = Nothing,
      eufArtworkFile = Nothing,
      eufAudioToken = mAudioToken
    }

--------------------------------------------------------------------------------

-- | Returns NotFound when the show slug doesn't exist in the DB.
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = do
  userInsert <- mkUserInsert "ep-new-post-missing-show" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMeta) <- liftIO $ expectSetupRight dbResult

    let form = minimalForm "0" Nothing Nothing
    result <- runExceptT $ action userModel userMeta (Slug "nonexistent-show") form

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Returns a validation error when the scheduled date is missing.
test_validationErrorForMissingSchedule :: TestDBConfig -> IO ()
test_validationErrorForMissingSchedule cfg = do
  userInsert <- mkUserInsert "ep-new-post-no-schedule" UserMetadata.Staff

  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Show For Missing Schedule",
            Shows.siSlug = Slug "show-for-missing-schedule",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId
      pure (userModel, userMeta, showId)
    (userModel, userMeta, _showId) <- liftIO $ expectSetupRight dbResult

    -- Form with no scheduled date
    let form = minimalForm "0" Nothing (Just "some-token")
    result <- runExceptT $ action userModel userMeta (Slug "show-for-missing-schedule") form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

-- | Returns a validation error when the audio token is missing.
test_validationErrorForMissingAudio :: TestDBConfig -> IO ()
test_validationErrorForMissingAudio cfg = do
  userInsert <- mkUserInsert "ep-new-post-no-audio" UserMetadata.Staff

  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Show For Missing Audio",
            Shows.siSlug = Slug "show-for-missing-audio",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId
      pure (userModel, userMeta, templateId)
    (userModel, userMeta, templateId) <- liftIO $ expectSetupRight dbResult

    -- Form with a valid schedule but no audio token
    let scheduleVal = Text.pack (show (ShowSchedule.unTemplateId templateId)) <> "|2026-03-01 10:00:00 UTC"
        form = minimalForm "0" (Just scheduleVal) Nothing
    result <- runExceptT $ action userModel userMeta (Slug "show-for-missing-audio") form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

-- | Staff user creates an episode with a pre-staged audio upload.
test_staffCreatesEpisode :: TestDBConfig -> IO ()
test_staffCreatesEpisode cfg = do
  userInsert <- mkUserInsert "ep-new-post-create" UserMetadata.Staff

  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Show For Episode Creation",
            Shows.siSlug = Slug "show-for-episode-creation",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    -- Set up user, show, schedule template, and staged upload
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId

      -- Create a staged upload record for the audio file
      let token = StagedUploads.Token "test-audio-token-ep-create"
          stagedInsert =
            StagedUploads.Insert
              { StagedUploads.siToken = token,
                StagedUploads.siUserId = userModel.mId,
                StagedUploads.siOriginalName = "episode.mp3",
                StagedUploads.siStoragePath = "temp/staging/test-audio-ep-create.mp3",
                StagedUploads.siMimeType = "audio/mpeg",
                StagedUploads.siFileSize = 1024,
                StagedUploads.siUploadType = StagedUploads.EpisodeAudio
              }
      _uploadId <- TRX.statement () (StagedUploads.insert stagedInsert)
      pure (userModel, userMeta, templateId)
    (userModel, userMeta, templateId) <- liftIO $ expectSetupRight dbResult

    -- Create the staging file on disk so the file move succeeds
    let stagingFilePath = "/tmp/kpbj/temp/staging/test-audio-ep-create.mp3"
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory stagingFilePath)
      writeFile stagingFilePath "fake audio content"

    -- Build form with valid schedule and audio token
    let scheduleVal = Text.pack (show (ShowSchedule.unTemplateId templateId)) <> "|2026-03-01 10:00:00 UTC"
        form = minimalForm "0" (Just scheduleVal) (Just "test-audio-token-ep-create")
    result <- runExceptT $ action userModel userMeta (Slug "show-for-episode-creation") form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Re-query to verify the episode was persisted.
    case result of
      Left _ -> pure ()
      Right episodeId -> do
        episodeResult <-
          runDB $
            TRX.transaction TRX.ReadCommitted TRX.Read $
              TRX.statement () (Episodes.getEpisodeById episodeId)
        liftIO $ do
          mEpisode <- expectSetupRight episodeResult
          mEpisode `shouldSatisfy` \case
            Nothing -> False
            Just ep -> ep.description == Just "Test episode description."
