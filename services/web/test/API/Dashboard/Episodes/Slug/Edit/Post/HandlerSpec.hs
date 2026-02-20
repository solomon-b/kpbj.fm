{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Post.Handler (action)
import API.Dashboard.Episodes.Slug.Edit.Post.Route (EpisodeEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
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

--------------------------------------------------------------------------------

-- | Minimal edit form that only sets a description.
descriptionEditForm :: Text -> EpisodeEditForm
descriptionEditForm desc =
  EpisodeEditForm
    { eefDescription = Just desc,
      eefTags = Nothing,
      eefScheduledDate = Nothing,
      eefTracksJson = Nothing,
      eefDurationSeconds = Nothing,
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
                Episodes.eiScheduleTemplateId = templateId,
                Episodes.eiScheduledAt = read "2026-03-01 10:00:00 UTC",
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
                Episodes.eiScheduleTemplateId = templateId,
                Episodes.eiScheduledAt = read "2026-03-01 10:00:00 UTC",
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
