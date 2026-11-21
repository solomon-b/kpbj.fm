{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "staff user deletes an event, it is gone from the DB" test_staffDeletesEvent

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID yields NotFound "Event".
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (Events.Id 99999)

    liftIO $ case result of
      Left (NotFound "Event") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Event\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | A staff user can delete an event; it is gone from the DB afterwards.
test_staffDeletesEvent :: TestDBConfig -> IO ()
test_staffDeletesEvent cfg = do
  userInsert <- mkUserInsert "ev-delete-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Event To Delete",
                Events.eiSlug = Slug.mkSlug "event-to-delete",
                Events.eiDescription = "This event will be deleted.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Delete Venue",
                Events.eiLocationAddress = "1 Delete St",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = userModel.mId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiTicketUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      insertTestEvent eventInsert
    eventId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action eventId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err
      Right () -> pure ()

    -- Verify the event is gone from the DB.
    lookupResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Events.getEventById eventId)

    liftIO $ do
      mEvent <- expectSetupRight lookupResult
      mEvent `shouldBe` Nothing
