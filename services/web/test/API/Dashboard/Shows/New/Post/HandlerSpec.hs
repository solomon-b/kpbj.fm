module API.Dashboard.Shows.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Post.Handler (action)
import API.Dashboard.Shows.New.Post.Route (NewShowForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (sort)
import Data.Text (Text)
import Data.Time (DayOfWeek (..), TimeOfDay (..), getCurrentTime)
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (LocalTime (..), utcToPacific)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
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
        it "creates no show when a schedule insert fails" test_failedScheduleInsertCreatesNoShow
        it "writes the schedule, the host, and the tag in one go" test_createsShowWithScheduleHostAndTag

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
      nsfSchedulesJson = Nothing,
      nsfScheduleStartDate = Nothing
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

-- | A failed schedule insert creates no show at all.
--
-- The show row, its hosts, its tags, and its schedule all commit in one transaction.
-- Run as separate statements the show row would survive a failed schedule insert, and
-- the retry would then be rejected by the slug uniqueness check, so staff would have
-- to delete the broken show before trying again.
--
-- The submitted slot carries @weeksOfMonth [6]@, which nothing upstream rejects. The
-- @weeks_of_month@ CHECK on @schedule_templates@ rejects it at the insert.
test_failedScheduleInsertCreatesNoShow :: TestDBConfig -> IO ()
test_failedScheduleInsertCreatesNoShow cfg = do
  let form =
        (minimalForm "Rolled Back Show" "active")
          { nsfSchedulesJson =
              Just "[{\"dayOfWeek\":\"monday\",\"weeksOfMonth\":[6],\"startTime\":\"20:00\",\"duration\":120,\"replayTime\":null}]"
          }
      expectedSlug = Slug "rolled-back-show"

  bracketAppM cfg $ do
    result <- runExceptT $ action form

    liftIO $ case result of
      Left (DatabaseError _) -> pure ()
      Left err -> expectationFailure $ "Expected a DatabaseError but got: " <> show err
      Right _ -> expectationFailure "Expected the creation to fail on the schedule insert"

    showResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug expectedSlug)

    liftIO $ do
      mShow <- expectSetupRight showResult
      -- A show here means the insert committed without its schedule.
      mShow `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

-- | A full form writes the show, its host, its tag, and its schedule together.
--
-- Every other case in this module submits 'minimalForm', which carries no hosts, no
-- tags, and no schedule, so 'assignHostsToShow', 'processShowTags', and
-- 'createSchedulesForShow' never do any work. This is the only test that runs them.
--
-- It also covers the role promotion, which is the one part of the creation
-- transaction that writes outside the show's own tables.
test_createsShowWithScheduleHostAndTag :: TestDBConfig -> IO ()
test_createsShowWithScheduleHostAndTag cfg = do
  -- A plain User, so assigning them as a host has to promote them to Host.
  userInsert <- mkUserInsert "new-full-show-host" UserMetadata.User
  now <- getCurrentTime
  let pacificToday = localDay (utcToPacific now)
      expectedSlug = Slug "full-show"

  bracketAppM cfg $ do
    setupResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser userInsert
    hostId <- liftIO $ expectSetupRight setupResult

    let form =
          (minimalForm "Full Show" "active")
            { nsfHosts = [hostId],
              nsfTags = Just "jazz, late night",
              nsfSchedulesJson =
                Just "[{\"dayOfWeek\":\"saturday\",\"weeksOfMonth\":[1,3],\"startTime\":\"22:00\",\"duration\":120,\"replayTime\":null}]"
            }

    result <- runExceptT $ action form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right createdShow -> Shows.slug createdShow `shouldBe` expectedSlug

    afterResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
      mShow <- TRX.statement () (Shows.getShowBySlug expectedSlug)
      case mShow of
        Nothing -> pure Nothing
        Just s -> do
          templates <- TRX.statement () (ShowSchedule.getScheduleTemplatesForShow (Shows.id s))
          validities <-
            traverse (\t -> TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate t.stId)) templates
          hosts <- TRX.statement () (ShowHost.getShowHosts (Shows.id s))
          tags <- TRX.statement () (Shows.getTagsForShow (Shows.id s))
          mMeta <- TRX.statement () (UserMetadata.getUserMetadata hostId)
          pure (Just (templates, concat validities, hosts, tags, mMeta))

    liftIO $ do
      after' <- expectSetupRight afterResult
      case after' of
        Nothing -> expectationFailure "Expected the show to exist"
        Just (templates, validities, hosts, tags, mMeta) -> do
          -- The submitted slot, with the duration turned into an end time.
          map (.stDayOfWeek) templates `shouldBe` [Just Saturday]
          map (.stStartTime) templates `shouldBe` [TimeOfDay 22 0 0]
          map (.stEndTime) templates `shouldBe` [TimeOfDay 0 0 0]
          map (.stWeeksOfMonth) templates `shouldBe` [Just [1, 3]]

          -- Open-ended from today. A template with no validity never airs.
          map (.stvEffectiveFrom) validities `shouldBe` [pacificToday]
          map (.stvEffectiveUntil) validities `shouldBe` [Nothing]

          map (.shmUserId) hosts `shouldBe` [hostId]
          sort (map ShowTags.stName tags) `shouldBe` ["jazz", "late night"]

          -- Assigning a plain User as a host promotes them.
          fmap (.mUserRole) mMeta `shouldBe` Just UserMetadata.Host
