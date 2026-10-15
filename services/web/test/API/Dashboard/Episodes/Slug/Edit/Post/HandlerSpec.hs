{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Post.Handler (action)
import API.Dashboard.Episodes.Slug.Edit.Post.Route (EpisodeEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay (..), fromGregorian)
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
import Test.Database.Helpers (addTestShowHost, insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Episodes.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent episode" test_notFoundForMissingEpisode
        it "updates episode description when staff submits valid form" test_updatesDescription
        it "returns NotAuthorized for unrelated regular user" test_notAuthorizedForUnrelatedUser
        it "rejects new audio too long for the slot and leaves the upload pending" test_rejectsOverLengthNewAudio
        it "rejects a move into a slot too short for the stored audio" test_rejectsMoveIntoShorterSlot

--------------------------------------------------------------------------------

-- | Minimal edit form that only sets a description.
descriptionEditForm :: Text -> EpisodeEditForm
descriptionEditForm desc =
  EpisodeEditForm
    { eefDescription = Just desc,
      eefTags = Nothing,
      eefScheduledDate = Nothing,
      eefTracksJson = Nothing,
      eefArtworkFile = Nothing,
      eefAudioClear = False,
      eefArtworkClear = False,
      eefAudioToken = Nothing
    }

--------------------------------------------------------------------------------

-- | Calling action with a slug and episode number that don't exist yields NotFound.
test_notFoundForMissingEpisode :: TestDBConfig -> IO ()
test_notFoundForMissingEpisode cfg = do
  userInsert <- mkUserInsert "ep-edit-post-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)

    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <-
      runExceptT $
        action
          userModel
          userMetaModel
          (Slug "no-such-show-slug")
          (Episodes.EpisodeNumber 999)
          (descriptionEditForm "Updated description")

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Staff submitting a valid description update persists the change in the database.
test_updatesDescription :: TestDBConfig -> IO ()
test_updatesDescription cfg = do
  userInsert <- mkUserInsert "ep-edit-post-desc" UserMetadata.Staff

  let showSlug = Slug "ep-edit-post-desc-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Post Desc Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      let episodeInsert =
            Episodes.Insert
              { Episodes.eiId = showId,
                Episodes.eiDescription = Just "Original description",
                Episodes.eiAudioFilePath = Nothing,
                Episodes.eiAudioFileSize = Nothing,
                Episodes.eiAudioMimeType = Nothing,
                Episodes.eiDurationSeconds = Nothing,
                Episodes.eiArtworkUrl = Nothing,
                Episodes.eiScheduleTemplateId = Just templateId,
                Episodes.eiAirDate = Just (read "2026-03-01"),
                Episodes.eiCreatedBy = userModel.mId
              }
      episodeId <- insertTestEpisode episodeInsert
      episodeModel <-
        TRX.statement () (Episodes.getEpisodeById episodeId)
          >>= maybe (error "episode not found after insert") pure
      pure (userModel, userMetaModel, episodeModel)

    (userModel, userMetaModel, episodeModel) <- liftIO $ expectSetupRight dbResult

    let newDescription = "Updated description for the episode"
    result <-
      runExceptT $
        action
          userModel
          userMetaModel
          showSlug
          episodeModel.episodeNumber
          (descriptionEditForm newDescription)

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the description was persisted in the database
    updatedEpisodeResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Episodes.getEpisodeById episodeModel.id)

    liftIO $ do
      updatedEpisodeResult' <- expectSetupRight updatedEpisodeResult
      case updatedEpisodeResult' of
        Nothing -> expectationFailure "Expected updated episode to exist in DB but got Nothing"
        Just ep -> ep.description `shouldBe` Just newDescription

-- | A User-role account that is not a show host and did not create the episode
-- receives NotAuthorized when attempting to edit.
test_notAuthorizedForUnrelatedUser :: TestDBConfig -> IO ()
test_notAuthorizedForUnrelatedUser cfg = do
  creatorInsert <- mkUserInsert "ep-edit-post-creator" UserMetadata.Host
  editorInsert <- mkUserInsert "ep-edit-post-editor" UserMetadata.User

  let showSlug = Slug "ep-edit-post-authz-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Post Authz Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      -- Creator: the host who owns the show and created the episode
      creatorId <- insertTestUser creatorInsert
      (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId creatorId
      let episodeInsert =
            Episodes.Insert
              { Episodes.eiId = showId,
                Episodes.eiDescription = Just "Original description",
                Episodes.eiAudioFilePath = Nothing,
                Episodes.eiAudioFileSize = Nothing,
                Episodes.eiAudioMimeType = Nothing,
                Episodes.eiDurationSeconds = Nothing,
                Episodes.eiArtworkUrl = Nothing,
                Episodes.eiScheduleTemplateId = Just templateId,
                Episodes.eiAirDate = Just (read "2026-03-01"),
                Episodes.eiCreatedBy = creatorId
              }
      episodeId <- insertTestEpisode episodeInsert
      episodeModel <-
        TRX.statement () (Episodes.getEpisodeById episodeId)
          >>= maybe (error "episode not found after insert") pure
      -- Editor: an unrelated User with no hosting assignment
      (editorModel, editorMetaModel) <- setupUserModels editorInsert
      pure (editorModel, editorMetaModel, episodeModel)

    (editorModel, editorMetaModel, episodeModel) <- liftIO $ expectSetupRight dbResult

    result <-
      runExceptT $
        action
          editorModel
          editorMetaModel
          showSlug
          episodeModel.episodeNumber
          (descriptionEditForm "Unauthorized edit attempt")

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"

-- | An episode in the default 1 hour slot, with 50:00 of audio already stored.
--
-- Returns the user, the show id, the slot's template id, and the episode.
setupEpisodeWithAudio ::
  UserMetadata.UserWithMetadataInsert ->
  Shows.Insert ->
  TRX.Transaction (User.Model, UserMetadata.Model, Shows.Id, ShowSchedule.TemplateId, Episodes.Model)
setupEpisodeWithAudio userInsert showInsert = do
  (userModel, userMetaModel) <- setupUserModels userInsert
  (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
  addTestShowHost showId userModel.mId
  let episodeInsert =
        Episodes.Insert
          { Episodes.eiId = showId,
            Episodes.eiDescription = Just "Original description",
            Episodes.eiAudioFilePath = Just "audio/episodes/2026/03/02/stored-episode.mp3",
            Episodes.eiAudioFileSize = Nothing,
            Episodes.eiAudioMimeType = Just "audio/mpeg",
            Episodes.eiDurationSeconds = Just 3000,
            Episodes.eiArtworkUrl = Nothing,
            Episodes.eiScheduleTemplateId = Just templateId,
            Episodes.eiAirDate = Just (fromGregorian 2026 3 2),
            Episodes.eiCreatedBy = userModel.mId
          }
  episodeId <- insertTestEpisode episodeInsert
  episodeModel <-
    TRX.statement () (Episodes.getEpisodeById episodeId)
      >>= maybe (error "episode not found after insert") pure
  pure (userModel, userMetaModel, showId, templateId, episodeModel)

-- | New audio one second over 58:00 does not fit the 1 hour slot.
--
-- The refusal comes before every write. The upload stays pending, and the
-- episode keeps its description and its stored length.
test_rejectsOverLengthNewAudio :: TestDBConfig -> IO ()
test_rejectsOverLengthNewAudio cfg = do
  userInsert <- mkUserInsert "ep-edit-post-too-long" UserMetadata.Staff

  let showSlug = Slug "ep-edit-post-too-long-show"
      showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Post Too Long Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      token = StagedUploads.Token "test-audio-token-ep-edit-too-long"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel, _showId, _templateId, episodeModel) <- setupEpisodeWithAudio userInsert showInsert
      let stagedInsert =
            StagedUploads.Insert
              { StagedUploads.siToken = token,
                StagedUploads.siUserId = userModel.mId,
                StagedUploads.siOriginalName = "episode-too-long.mp3",
                StagedUploads.siStoragePath = "temp/staging/test-audio-ep-edit-too-long.mp3",
                StagedUploads.siMimeType = "audio/mpeg",
                StagedUploads.siFileSize = 55680000,
                StagedUploads.siDurationSeconds = Just 3481,
                StagedUploads.siUploadType = StagedUploads.EpisodeAudio
              }
      _uploadId <- TRX.statement () (StagedUploads.insert stagedInsert)
      pure (userModel, userMetaModel, episodeModel)

    (userModel, userMetaModel, episodeModel) <- liftIO $ expectSetupRight dbResult

    let form =
          (descriptionEditForm "Edited description")
            { eefAudioToken = Just "test-audio-token-ep-edit-too-long"
            }
    result <- runExceptT $ action userModel userMetaModel showSlug episodeModel.episodeNumber form

    liftIO $ case result of
      Left (ValidationError message) ->
        message `shouldBe` "This episode runs 58:01. Episodes in this 1 hour slot can run at most 58:00, so the station break fits. Trim the audio and upload it again."
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

    afterResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $ do
          mStaged <- TRX.statement () (StagedUploads.getByToken token)
          mEpisode <- TRX.statement () (Episodes.getEpisodeById episodeModel.id)
          pure (mStaged, mEpisode)
    liftIO $ do
      (mStaged, mEpisode) <- expectSetupRight afterResult
      fmap (.status) mStaged `shouldBe` Just StagedUploads.Pending
      fmap (.description) mEpisode `shouldBe` Just (Just "Original description")
      fmap (.durationSeconds) mEpisode `shouldBe` Just (Just 3000)

-- | 50:00 of stored audio fits the 1 hour slot but not a 30 minute slot.
--
-- A move into the shorter slot is refused, and the episode stays where it was.
test_rejectsMoveIntoShorterSlot :: TestDBConfig -> IO ()
test_rejectsMoveIntoShorterSlot cfg = do
  userInsert <- mkUserInsert "ep-edit-post-short-slot" UserMetadata.Staff

  let showSlug = Slug "ep-edit-post-short-slot-show"
      showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Post Short Slot Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel, showId, templateId, episodeModel) <- setupEpisodeWithAudio userInsert showInsert
      -- A second slot for the same show: Tuesdays, 10:00 to 10:30.
      shortTemplateId <-
        TRX.statement () $
          ShowSchedule.insertScheduleTemplate
            defaultScheduleInsert
              { ShowSchedule.stiShowId = showId,
                ShowSchedule.stiDayOfWeek = Tuesday,
                ShowSchedule.stiEndTime = TimeOfDay 10 30 0
              }
      pure (userModel, userMetaModel, templateId, shortTemplateId, episodeModel)

    (userModel, userMetaModel, templateId, shortTemplateId, episodeModel) <- liftIO $ expectSetupRight dbResult

    -- 2026-03-03 is a Tuesday.
    let scheduleValue = Text.pack (show (ShowSchedule.unTemplateId shortTemplateId)) <> "|2026-03-03"
        form = (descriptionEditForm "Original description") {eefScheduledDate = Just scheduleValue}
    result <- runExceptT $ action userModel userMetaModel showSlug episodeModel.episodeNumber form

    liftIO $ case result of
      Left (ValidationError message) ->
        message `shouldBe` "This episode runs 50:00. Episodes in this 30 minute slot can run at most 28:00, so the station break fits. Trim the audio and upload it again."
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

    afterResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Episodes.getEpisodeById episodeModel.id)
    liftIO $ do
      mEpisode <- expectSetupRight afterResult
      fmap (.scheduleTemplateId) mEpisode `shouldBe` Just (Just templateId)
