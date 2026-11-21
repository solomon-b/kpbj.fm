{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Get.Handler (EventEditViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.Slug.Edit.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "staff user can access the edit form for any event" test_staffCanAccessEditForm

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID yields NotFound "Event".
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg = do
  userInsert <- mkUserInsert "ev-edit-get-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      snd <$> setupUserModels userInsert
    userMetaModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel (Events.Id 99999)

    liftIO $ case result of
      Left (NotFound "Event") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Event\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | A staff user can access the edit form for an event they did not create.
test_staffCanAccessEditForm :: TestDBConfig -> IO ()
test_staffCanAccessEditForm cfg = do
  creatorInsert <- mkUserInsert "ev-edit-get-creator" UserMetadata.Staff
  staffInsert <- mkUserInsert "ev-edit-get-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      creatorId <- insertTestUser creatorInsert
      (_, staffMetaModel) <- setupUserModels staffInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Edit Get Staff Test Event",
                Events.eiSlug = Slug.mkSlug "edit-get-staff-test-event",
                Events.eiDescription = "An event created by a different user.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Staff Test Venue",
                Events.eiLocationAddress = "1 Staff Rd",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = creatorId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiTicketUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (staffMetaModel, eventId)
    (staffMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action staffMetaModel eventId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.eevEvent.emId `shouldBe` eventId
