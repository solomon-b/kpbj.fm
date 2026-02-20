module API.Dashboard.Shows.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Post.Handler (action)
import API.Dashboard.Shows.New.Post.Route (NewShowForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.New.Post.Handler" $ do
      describe "action" $ do
        it "returns ValidationError for empty title" test_validationErrorForEmptyTitle
        it "creates show in DB for valid form" test_createsShowWithValidForm
        it "creates show with inactive status" test_createsShowWithInactiveStatus
        it "returns Right for form with empty schedules JSON" test_createsShowWithEmptySchedules
        it "returns ValidationError for duplicate slug" test_validationErrorForDuplicateSlug

--------------------------------------------------------------------------------

-- | Minimal valid form with no logo, no hosts, and no schedules.
minimalForm :: Text -> Text -> NewShowForm
minimalForm title status =
  NewShowForm
    { nsfTitle = title,
      nsfDescription = "",
      nsfTags = Nothing,
      nsfLogoFile = Nothing,
      nsfStatus = status,
      nsfHosts = [],
      nsfSchedulesJson = Nothing
    }

--------------------------------------------------------------------------------

-- | Submitting a form with an empty title returns a ValidationError.
test_validationErrorForEmptyTitle :: TestDBConfig -> IO ()
test_validationErrorForEmptyTitle cfg = do
  let form = minimalForm "" "active"

  bracketAppM cfg $ do
    result <- runExceptT $ action form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

-- | Submitting a valid form creates a show in the database.
test_createsShowWithValidForm :: TestDBConfig -> IO ()
test_createsShowWithValidForm cfg = do
  let form = minimalForm "My New Show" "active"
  -- The handler generates a slug from the title: "my-new-show"
  let expectedSlug = Slug "my-new-show"

  bracketAppM cfg $ do
    result <- runExceptT $ action form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the show was created in the DB.
    showResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug expectedSlug)

    liftIO $ do
      showResult' <- expectSetupRight showResult
      showResult' `shouldSatisfy` \case
        Nothing -> False
        Just s -> Shows.title s == "My New Show" && Shows.status s == Shows.Active

-- | Submitting a form with "inactive" status creates an inactive show.
test_createsShowWithInactiveStatus :: TestDBConfig -> IO ()
test_createsShowWithInactiveStatus cfg = do
  let form = minimalForm "Inactive Show" "inactive"
  let expectedSlug = Slug "inactive-show"

  bracketAppM cfg $ do
    result <- runExceptT $ action form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    showResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug expectedSlug)

    liftIO $ do
      showResult' <- expectSetupRight showResult
      case showResult' of
        Nothing -> expectationFailure "Expected show to exist in DB but got Nothing"
        Just s -> Shows.status s `shouldBe` Shows.Inactive

-- | A form with an explicit empty schedules JSON list still creates the show.
test_createsShowWithEmptySchedules :: TestDBConfig -> IO ()
test_createsShowWithEmptySchedules cfg = do
  let form =
        (minimalForm "Show With Empty Schedules" "active")
          { nsfSchedulesJson = Just "[]"
          }
  let expectedSlug = Slug "show-with-empty-schedules"

  bracketAppM cfg $ do
    result <- runExceptT $ action form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the show was created and has no schedule templates.
    showResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
      mShow <- TRX.statement () (Shows.getShowBySlug expectedSlug)
      case mShow of
        Nothing -> pure (Nothing, [])
        Just s -> do
          templates <- TRX.statement () (ShowSchedule.getScheduleTemplatesForShow (Shows.id s))
          pure (Just s, templates)

    liftIO $ do
      (mShow, templates) <- expectSetupRight showResult
      mShow `shouldSatisfy` \case
        Nothing -> False
        Just s -> Shows.title s == "Show With Empty Schedules"
      length templates `shouldBe` 0

-- | Creating two shows with the same title (same derived slug) returns
-- a ValidationError rather than an opaque DatabaseError.
test_validationErrorForDuplicateSlug :: TestDBConfig -> IO ()
test_validationErrorForDuplicateSlug cfg = do
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Duplicate Slug Show",
            Shows.siSlug = Slug "duplicate-slug-show",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  bracketAppM cfg $ do
    -- Pre-seed a show with the slug that the form will derive.
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestShowWithSchedule showInsert defaultScheduleInsert
    _ <- liftIO $ expectSetupRight dbResult

    -- Attempt to create a show with the same title (same slug).
    let form = minimalForm "Duplicate Slug Show" "active"
    result <- runExceptT $ action form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"
