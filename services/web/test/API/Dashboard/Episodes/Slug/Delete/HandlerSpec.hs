{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Episodes.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent episode on a real show" test_notFoundForMissingEpisode
        it "soft-deletes the episode on success" test_archivesEpisode

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Calling action with a real show slug but a nonexistent episode number yields
-- NotFound "Episode".
test_notFoundForMissingEpisode :: TestDBConfig -> IO ()
test_notFoundForMissingEpisode cfg = do
  userInsert <- mkUserInsert "ep-delete-missing-ep" UserMetadata.Staff

  let showSlug = Slug "ep-delete-missing-ep-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Delete Missing Ep Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userId, showModel)

    (_userId, _showModel) <- liftIO $ expectSetupRight dbResult

    result <-
      runExceptT $
        action
          showSlug
          (Episodes.EpisodeNumber 99999)

    liftIO $ case result of
      Left (NotFound "Episode") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Episode\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Calling action with a valid show slug and episode number soft-deletes the
-- episode (sets deletedAt) so it no longer appears in published episode queries.
test_archivesEpisode :: TestDBConfig -> IO ()
test_archivesEpisode cfg = do
  userInsert <- mkUserInsert "ep-delete-archive" UserMetadata.Staff

  let showSlug = Slug "ep-delete-archive-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Delete Archive Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      (showId, templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      let episodeInsert =
            Episodes.Insert
              { Episodes.eiId = showId,
                Episodes.eiDescription = Just "Episode to be archived",
                Episodes.eiAudioFilePath = Nothing,
                Episodes.eiAudioFileSize = Nothing,
                Episodes.eiAudioMimeType = Nothing,
                Episodes.eiDurationSeconds = Nothing,
                Episodes.eiArtworkUrl = Nothing,
                Episodes.eiScheduleTemplateId = templateId,
                Episodes.eiScheduledAt = read "2026-03-01 10:00:00 UTC",
                Episodes.eiCreatedBy = userId
              }
      episodeId <- insertTestEpisode episodeInsert
      TRX.statement () (Episodes.getEpisodeById episodeId)
        >>= maybe (error "episode not found after insert") pure

    episodeModel <- liftIO $ expectSetupRight dbResult

    result <-
      runExceptT $
        action
          showSlug
          episodeModel.episodeNumber

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err
      Right () -> pure ()

    -- Verify the episode was soft-deleted: getEpisodeById filters out deleted
    -- records, so it should return Nothing after archiving.
    archivedResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Episodes.getEpisodeById episodeModel.id)

    liftIO $ do
      archivedResult' <- expectSetupRight archivedResult
      archivedResult' `shouldBe` Nothing
