{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (action)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ShowEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay (..), addDays, addUTCTime, getCurrentTime, nominalDay, utctDay)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
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
    describe "API.Dashboard.Shows.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent slug" test_notFoundForMissingSlug
        it "updates show title when valid form submitted" test_updatesShowTitle
        it "updates show description when valid form submitted" test_updatesShowDescription
        it "preserves existing logo when sefLogoClear is False and no new file" test_preservesLogoUrl
        it "allows an unrelated edit when another show holds the same slot from a future date" test_unchangedScheduleSkipsConflictCheck
        it "leaves a pending schedule intact when only the title changes" test_titleEditPreservesPendingSchedule
        it "closes the schedule windows when the show is set inactive" test_deactivateClosesScheduleWindow
        it "rolls the whole schedule change back when an insert fails" test_failedScheduleInsertRollsBack

--------------------------------------------------------------------------------

-- | Minimal valid edit form.
editForm :: Text -> Text -> ShowEditForm
editForm title status =
  ShowEditForm
    { sefTitle = title,
      sefDescription = "",
      sefTags = Nothing,
      sefLogoFile = Nothing,
      sefLogoClear = False,
      sefStatus = status,
      sefHosts = [],
      sefSchedulesJson = Nothing,
      sefScheduleStartDate = Nothing
    }

--------------------------------------------------------------------------------

-- | Calling action with a slug that does not exist in the DB yields NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "edit-notfound" UserMetadata.Staff
  let nonexistentSlug = Slug "no-such-show-slug"
  let form = editForm "Any Title" "active"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (UserMetadata.getUserMetadata userId)
        >>= maybe (error "metadata not found") pure

    userMetaModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel nonexistentSlug form

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Submitting a valid edit form updates the show title in the database.
test_updatesShowTitle :: TestDBConfig -> IO ()
test_updatesShowTitle cfg = do
  userInsert <- mkUserInsert "edit-title" UserMetadata.Staff

  let originalSlug = Slug "edit-title-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Original Title",
            Shows.siSlug = originalSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  let newTitle = "Updated Show Title"
  let form = editForm newTitle "active"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, showId)

    (userMetaModel, showModel, showId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Fetch by ID (stable) rather than by generated slug (brittle)
    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowById showId)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.title s `shouldBe` newTitle

-- | Submitting a form with a description stores it on the show.
test_updatesShowDescription :: TestDBConfig -> IO ()
test_updatesShowDescription cfg = do
  userInsert <- mkUserInsert "edit-desc" UserMetadata.Staff

  let showSlug = Slug "edit-desc-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Desc Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
  let form =
        (editForm "Edit Desc Show" "active")
          { sefDescription = "Updated description text."
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel)

    (userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug showSlug)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.description s `shouldBe` Just "Updated description text."

-- | When sefLogoClear is False and no new file is uploaded, the existing logo
-- URL is preserved on the show.
test_preservesLogoUrl :: TestDBConfig -> IO ()
test_preservesLogoUrl cfg = do
  userInsert <- mkUserInsert "edit-logo" UserMetadata.Staff

  let showSlug = Slug "edit-logo-show"
  let originalLogoUrl = Just "https://cdn.example.com/logo.png"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Logo Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = originalLogoUrl,
            Shows.siStatus = Shows.Active
          }
  let form =
        (editForm "Edit Logo Show" "active")
          { sefLogoClear = False,
            sefLogoFile = Nothing
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel)

    (userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug showSlug)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.logoUrl s `shouldBe` originalLogoUrl

-- | An edit that leaves the schedule alone is not conflict-checked, so it goes
-- through even when another show holds the same slot from a future date.
--
-- The edit form always re-posts the show's current slots. Conflict-checking every
-- edit would reject the title change below, because the other show's pending
-- booking overlaps the re-posted slot, and would leave this show uneditable.
test_unchangedScheduleSkipsConflictCheck :: TestDBConfig -> IO ()
test_unchangedScheduleSkipsConflictCheck cfg = do
  userInsert <- mkUserInsert "edit-pending-slot" UserMetadata.Staff
  today <- utctDay <$> getCurrentTime

  let showSlug = Slug "edit-pending-slot-show"
      showInsert =
        Shows.Insert
          { Shows.siTitle = "Thursday Night Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      otherShowInsert =
        Shows.Insert
          { Shows.siTitle = "Incoming Thursday Show",
            Shows.siSlug = Slug "edit-pending-slot-other-show",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      -- Both shows sit on Thursday 20:00-21:00.
      thursdayNight =
        defaultScheduleInsert
          { ShowSchedule.stiDayOfWeek = Just Thursday,
            ShowSchedule.stiWeeksOfMonth = Just [1, 2, 3, 4, 5],
            ShowSchedule.stiStartTime = TimeOfDay 20 0 0,
            ShowSchedule.stiEndTime = TimeOfDay 21 0 0
          }
      newTitle = "Thursday Night Show Renamed"
      -- The form re-posts the show's current slot verbatim. Only the title changes.
      form =
        (editForm newTitle "active")
          { sefSchedulesJson =
              Just "[{\"dayOfWeek\":\"thursday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"20:00\",\"duration\":60,\"replayTime\":null}]"
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure

      -- The show being edited holds the slot today.
      (showId, templateId) <- insertTestShowWithSchedule showInsert thursdayNight
      _ <-
        TRX.statement () $
          ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing)

      -- Another show holds the same slot from 30 days out, open-ended.
      (_otherShowId, otherTemplateId) <- insertTestShowWithSchedule otherShowInsert thursdayNight
      _ <-
        TRX.statement () $
          ShowSchedule.insertValidity (ShowSchedule.ValidityInsert otherTemplateId (addDays 30 today) Nothing)

      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, showId)

    (userMetaModel, showModel, showId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected the edit to succeed but got Left: " <> show err
      Right _ -> pure ()

    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowById showId)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.title s `shouldBe` newTitle

-- | A title-only edit must not destroy the show's pending schedule.
--
-- The edit form is populated from the pending templates when a pending schedule
-- exists, so an unrelated save re-posts the pending slots verbatim. Diffing those
-- against the active templates reports a change, which cancelled the pending and
-- orphaned any episode already booked against it. Cancelling collapses the validity
-- to the empty window @[from, from)@, which no date satisfies, so those episodes go
-- silent with no warning.
test_titleEditPreservesPendingSchedule :: TestDBConfig -> IO ()
test_titleEditPreservesPendingSchedule cfg = do
  userInsert <- mkUserInsert "edit-keeps-pending" UserMetadata.Staff
  today <- utctDay <$> getCurrentTime

  let changeoverDate = addDays 30 today
      showInsert =
        Shows.Insert
          { Shows.siTitle = "Monday Morning Show",
            Shows.siSlug = Slug "edit-keeps-pending-show",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      -- Currently airing slot, closed out on the changeover date.
      mondayMorning =
        defaultScheduleInsert
          { ShowSchedule.stiDayOfWeek = Just Monday,
            ShowSchedule.stiWeeksOfMonth = Just [1, 2, 3, 4, 5],
            ShowSchedule.stiStartTime = TimeOfDay 9 0 0,
            ShowSchedule.stiEndTime = TimeOfDay 10 0 0
          }
      -- Pending slot that takes over on the changeover date.
      tuesdayAfternoon =
        mondayMorning
          { ShowSchedule.stiDayOfWeek = Just Tuesday,
            ShowSchedule.stiStartTime = TimeOfDay 15 0 0,
            ShowSchedule.stiEndTime = TimeOfDay 16 0 0
          }
      newTitle = "Monday Morning Show Renamed"
      -- What the edit form posts back: the pending slot and its start date, untouched.
      form =
        (editForm newTitle "active")
          { sefSchedulesJson =
              Just "[{\"dayOfWeek\":\"tuesday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"15:00\",\"duration\":60,\"replayTime\":null}]",
            sefScheduleStartDate = Just (Text.pack (show changeoverDate))
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure

      (showId, activeTemplateId) <- insertTestShowWithSchedule showInsert mondayMorning
      _ <-
        TRX.statement () $
          ShowSchedule.insertValidity
            (ShowSchedule.ValidityInsert activeTemplateId (addDays (-30) today) (Just changeoverDate))

      pendingTemplateId <-
        TRX.statement () $
          ShowSchedule.insertScheduleTemplate tuesdayAfternoon {ShowSchedule.stiShowId = showId}
      _ <-
        TRX.statement () $
          ShowSchedule.insertValidity (ShowSchedule.ValidityInsert pendingTemplateId changeoverDate Nothing)

      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, pendingTemplateId)

    (userMetaModel, showModel, pendingTemplateId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected the edit to succeed but got Left: " <> show err
      Right _ -> pure ()

    validityResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate pendingTemplateId)

    liftIO $ do
      validities <- expectSetupRight validityResult
      case validities of
        [] -> expectationFailure "Expected the pending template to still have a validity period"
        (v : _) ->
          -- Still open-ended. A cancelled pending would read Just changeoverDate here.
          v.stvEffectiveUntil `shouldBe` Nothing

-- | Setting a show inactive closes its schedule windows.
--
-- The close runs as step 9, after the schedule diff in step 8. Step 8 recreates any
-- submitted slot it cannot find among the active templates, so a close placed before
-- it would be undone on the same save. This test fails if the two steps swap.
--
-- An inactive show that keeps an open window can be reactivated onto a slot another
-- show has taken since, because the conflict check skips inactive shows.
test_deactivateClosesScheduleWindow :: TestDBConfig -> IO ()
test_deactivateClosesScheduleWindow cfg = do
  userInsert <- mkUserInsert "edit-deactivate" UserMetadata.Staff
  today <- utctDay <$> getCurrentTime

  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Retiring Show",
            Shows.siSlug = Slug "edit-deactivate-show",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      wednesdayNoon =
        defaultScheduleInsert
          { ShowSchedule.stiDayOfWeek = Just Wednesday,
            ShowSchedule.stiWeeksOfMonth = Just [1, 2, 3, 4, 5],
            ShowSchedule.stiStartTime = TimeOfDay 12 0 0,
            ShowSchedule.stiEndTime = TimeOfDay 13 0 0
          }
      -- The form re-posts the show's current slot. Only the status changes.
      form =
        (editForm "Retiring Show" "inactive")
          { sefSchedulesJson =
              Just "[{\"dayOfWeek\":\"wednesday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"12:00\",\"duration\":60,\"replayTime\":null}]"
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure

      (showId, templateId) <- insertTestShowWithSchedule showInsert wednesdayNoon
      _ <-
        TRX.statement () $
          ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing)

      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, showId)

    (userMetaModel, showModel, showId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected the edit to succeed but got Left: " <> show err
      Right _ -> pure ()

    afterResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $ do
          templates <- TRX.statement () (ShowSchedule.getScheduleTemplatesForShow showId)
          validities <- traverse (\t -> TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate t.stId)) templates
          pure (concat validities)

    liftIO $ do
      validities <- expectSetupRight afterResult
      case validities of
        [] -> expectationFailure "Expected the show to still have a validity period"
        _ ->
          -- Every window is closed. An open one means step 9 ran before step 8, or
          -- did not run at all.
          filter (\v -> isNothing v.stvEffectiveUntil) validities `shouldBe` []

-- | A failed template insert leaves the show's schedule and episodes untouched.
--
-- The schedule diff ends the removed template's validity and clears both
-- @schedule_template_id@ and @scheduled_at@ from its upcoming episodes, then inserts
-- the replacement. Run as separate statements those removals commit on their own, so
-- a failed insert leaves the show with no schedule and its episodes stripped of their
-- air times, with nothing to recover them from. They all run in one transaction now.
--
-- The submitted slot carries @weeksOfMonth [6]@, which nothing upstream rejects.
-- 'parseScheduleSlot' never validates the weeks, 'validateNoOverlaps' sees one slot,
-- and no other show holds a week 6. The @weeks_of_month@ CHECK on @schedule_templates@
-- rejects it at the insert, which is the failure the transaction has to contain.
test_failedScheduleInsertRollsBack :: TestDBConfig -> IO ()
test_failedScheduleInsertRollsBack cfg = do
  userInsert <- mkUserInsert "edit-rollback" UserMetadata.Staff
  now <- getCurrentTime
  let today = utctDay now
      episodeAirsAt = addUTCTime (7 * nominalDay) now

  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Rollback Show",
            Shows.siSlug = Slug "edit-rollback-show",
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
      mondayEvening =
        defaultScheduleInsert
          { ShowSchedule.stiDayOfWeek = Just Monday,
            ShowSchedule.stiWeeksOfMonth = Just [1, 2, 3, 4, 5],
            ShowSchedule.stiStartTime = TimeOfDay 20 0 0,
            ShowSchedule.stiEndTime = TimeOfDay 22 0 0
          }
      -- Same day and time, different weeks, so the diff reads it as one slot
      -- removed and one added.
      form =
        (editForm "Rollback Show" "active")
          { sefSchedulesJson =
              Just "[{\"dayOfWeek\":\"monday\",\"weeksOfMonth\":[6],\"startTime\":\"20:00\",\"duration\":120,\"replayTime\":null}]"
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure

      (showId, templateId) <- insertTestShowWithSchedule showInsert mondayEvening
      validityId <-
        TRX.statement () $
          ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing)

      episodeId <-
        insertTestEpisode
          Episodes.Insert
            { Episodes.eiId = showId,
              Episodes.eiDescription = Nothing,
              Episodes.eiAudioFilePath = Nothing,
              Episodes.eiAudioFileSize = Nothing,
              Episodes.eiAudioMimeType = Nothing,
              Episodes.eiDurationSeconds = Nothing,
              Episodes.eiArtworkUrl = Nothing,
              Episodes.eiScheduleTemplateId = Just templateId,
              Episodes.eiScheduledAt = Just episodeAirsAt,
              Episodes.eiCreatedBy = userId
            }

      -- Read the air time back rather than reusing episodeAirsAt. Postgres stores
      -- microseconds and getCurrentTime gives nanoseconds, so the two differ.
      storedAirsAt <-
        TRX.statement () (Episodes.getEpisodeById episodeId)
          >>= maybe (error "episode not found") (pure . (.scheduledAt))

      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, templateId, validityId, episodeId, storedAirsAt)

    (userMetaModel, showModel, templateId, _validityId, episodeId, storedAirsAt) <-
      liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left (DatabaseError _) -> pure ()
      Left err -> expectationFailure $ "Expected a DatabaseError but got: " <> show err
      Right _ -> expectationFailure "Expected the edit to fail on the schedule insert"

    afterResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $ do
          validities <- TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate templateId)
          episode <- TRX.statement () (Episodes.getEpisodeById episodeId)
          pure (validities, episode)

    liftIO $ do
      (validities, mEpisode) <- expectSetupRight afterResult

      -- The original slot is still open. An end date here means the removal committed
      -- without its replacement.
      map (.stvEffectiveUntil) validities `shouldBe` [Nothing]

      case mEpisode of
        Nothing -> expectationFailure "Expected the episode to still exist"
        Just episode -> do
          -- The episode keeps its slot. A NULL here is unrecoverable: nothing records
          -- what the air time used to be.
          episode.scheduleTemplateId `shouldBe` Just templateId
          episode.scheduledAt `shouldBe` storedAirsAt
