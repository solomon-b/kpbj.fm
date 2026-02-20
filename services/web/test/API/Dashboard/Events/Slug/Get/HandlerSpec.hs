{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Get.Handler (EventDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
    describe "API.Dashboard.Events.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "returns event detail including author for a valid event" test_returnsEventDetail

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID yields NotFound "Event".
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg = do
  userInsert <- mkUserInsert "ev-detail-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Events.Id 99999)

    liftIO $ case result of
      Left (NotFound "Event") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Event\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | After inserting an event, action returns the event detail with the correct
-- event ID and an author entry.
test_returnsEventDetail :: TestDBConfig -> IO ()
test_returnsEventDetail cfg = do
  userInsert <- mkUserInsert "ev-detail-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Detail Test Event",
                Events.eiSlug = Slug.mkSlug "detail-test-event",
                Events.eiDescription = "Description for detail test.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Detail Venue",
                Events.eiLocationAddress = "789 Detail Rd",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = userModel.mId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (userModel, userMetaModel, eventId)
    (userModel, userMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel eventId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        vd.edvEvent.emId `shouldBe` eventId
        vd.edvAuthor `shouldBe` Just userMetaModel
