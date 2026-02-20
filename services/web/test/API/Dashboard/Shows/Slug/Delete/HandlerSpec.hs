{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent slug" test_targetShowNotFound
        it "deletes an existing show and returns its ID and title" test_deleteSuccess
        it "show cannot be found again after deletion" test_deletedShowNotFoundAgain

--------------------------------------------------------------------------------

-- | Insert a show and return its model.
insertShowFixture :: Text -> Text -> TRX.Transaction Shows.Model
insertShowFixture titleText slugText = do
  let showInsert =
        Shows.Insert
          { Shows.siTitle = titleText,
            Shows.siSlug = Slug slugText,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
  (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
  TRX.statement () (Shows.getShowById showId)
    >>= maybe (error "insertShowFixture: show not found") pure

--------------------------------------------------------------------------------

-- | Calling action with a slug that does not exist returns NotFound.
test_targetShowNotFound :: TestDBConfig -> IO ()
test_targetShowNotFound cfg = do
  let nonexistentSlug = Slug "does-not-exist-slug"

  bracketAppM cfg $ do
    result <- runExceptT $ action nonexistentSlug

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Calling action with an existing show's slug returns the deleted show's
-- ID and title.
test_deleteSuccess :: TestDBConfig -> IO ()
test_deleteSuccess cfg = do
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertShowFixture "Delete Me Show" "delete-me-show"

    showModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action showModel.slug

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right (deletedId, deletedTitle) -> do
        deletedId `shouldBe` showModel.id
        deletedTitle `shouldBe` showModel.title

-- | After a successful deletion, calling action again with the same slug returns
-- NotFound because the soft-deleted show is excluded from lookups.
test_deletedShowNotFoundAgain :: TestDBConfig -> IO ()
test_deletedShowNotFoundAgain cfg = do
  let targetSlug = Slug "delete-twice-show"

  bracketAppM cfg $ do
    -- Insert and then delete the show.
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertShowFixture "Delete Twice Show" "delete-twice-show"

    showModel <- liftIO $ expectSetupRight dbResult

    firstResult <- runExceptT $ action showModel.slug

    liftIO $ case firstResult of
      Right _ -> pure ()
      Left err -> expectationFailure $ "First delete should succeed but got: " <> show err

    -- Attempt to delete the same show again.
    secondResult <- runExceptT $ action targetSlug

    liftIO $ case secondResult of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound on second delete but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound on second delete but got Right"
