{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Post.Handler (action)
import API.Dashboard.Events.Slug.Edit.Post.Route (EventEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "staff user can update an event title" test_updatesEventTitle
        it "unrelated regular user gets NotAuthorized" test_notAuthorizedForUnrelatedUser

--------------------------------------------------------------------------------

-- | A valid edit form with sensible defaults.
validEditForm :: EventEditForm
validEditForm =
  EventEditForm
    { eefTitle = "Updated Event Title",
      eefDescription = "Updated description text.",
      eefStartsAt = "2026-06-15T10:00",
      eefEndsAt = "2026-06-15T14:00",
      eefLocationName = "Updated Venue",
      eefLocationAddress = "999 Updated St",
      eefStatus = "published",
      eefPosterImage = Nothing,
      eefPosterImageClear = False,
      eefFeaturedOnHomepage = "false"
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID yields NotFound "Event".
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg = do
  userInsert <- mkUserInsert "ev-edit-post-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Events.Id 99999) (Slug.mkSlug "no-event") validEditForm

    liftIO $ case result of
      Left (NotFound "Event") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Event\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | A staff user can update the title of any event.
test_updatesEventTitle :: TestDBConfig -> IO ()
test_updatesEventTitle cfg = do
  creatorInsert <- mkUserInsert "ev-edit-post-creator" UserMetadata.Staff
  staffInsert <- mkUserInsert "ev-edit-post-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      creatorId <- insertTestUser creatorInsert
      (staffModel, staffMetaModel) <- setupUserModels staffInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Original Event Title",
                Events.eiSlug = Slug.mkSlug "original-event-title",
                Events.eiDescription = "Original description.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Original Venue",
                Events.eiLocationAddress = "1 Original Rd",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = creatorId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (staffModel, staffMetaModel, eventId)
    (staffModel, staffMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action staffModel staffMetaModel eventId (Slug.mkSlug "original-event-title") validEditForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the event title was updated in the DB.
    lookupResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Events.getEventById eventId)

    liftIO $ do
      mEvent <- expectSetupRight lookupResult
      case mEvent of
        Nothing -> assertFailure "Event not found after update"
        Just updatedEvent -> updatedEvent.emTitle `shouldBe` "Updated Event Title"

-- | A regular User who did not create the event gets NotAuthorized.
test_notAuthorizedForUnrelatedUser :: TestDBConfig -> IO ()
test_notAuthorizedForUnrelatedUser cfg = do
  creatorInsert <- mkUserInsert "ev-edit-post-owner" UserMetadata.Staff
  otherInsert <- mkUserInsert "ev-edit-post-other" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      creatorId <- insertTestUser creatorInsert
      (otherModel, otherMetaModel) <- setupUserModels otherInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Edit Post Unauth Event",
                Events.eiSlug = Slug.mkSlug "edit-post-unauth-event",
                Events.eiDescription = "Created by someone else.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Unauth Venue",
                Events.eiLocationAddress = "2 Unauth Rd",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = creatorId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (otherModel, otherMetaModel, eventId)
    (otherModel, otherMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action otherModel otherMetaModel eventId (Slug.mkSlug "edit-post-unauth-event") validEditForm

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
