module API.Dashboard.Events.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Post.Handler (action)
import API.Dashboard.Events.New.Post.Route (NewEventForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.New.Post.Handler" $ do
      describe "action" $ do
        it "creates an event with valid form data" test_createsEvent
        it "fails when end time is before start time" test_failsOnInvalidDates

--------------------------------------------------------------------------------

-- | A valid event form with start before end.
validEventForm :: NewEventForm
validEventForm =
  NewEventForm
    { nefTitle = "Summer Block Party",
      nefDescription = "A great community event.",
      nefStartsAt = "2026-06-15T10:00",
      nefEndsAt = "2026-06-15T14:00",
      nefLocationName = "Community Park",
      nefLocationAddress = "456 Park Ave, Burbank, CA",
      nefStatus = "published",
      nefPosterImage = Nothing,
      nefFeaturedOnHomepage = "false"
    }

--------------------------------------------------------------------------------

-- | Staff user submits valid form data, event is created and exists in the DB.
test_createsEvent :: TestDBConfig -> IO ()
test_createsEvent cfg = do
  userInsert <- mkUserInsert "ev-new-post-create" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel validEventForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the event exists in the DB by querying for all events
    lookupResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Events.getAllEvents 50 0)

    liftIO $ do
      events <- expectSetupRight lookupResult
      length events `shouldBe` 1

-- | Submitting a form where end time is before start time results in a Left error.
test_failsOnInvalidDates :: TestDBConfig -> IO ()
test_failsOnInvalidDates cfg = do
  userInsert <- mkUserInsert "ev-new-post-dates" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    let badForm =
          validEventForm
            { nefStartsAt = "2026-06-15T14:00",
              nefEndsAt = "2026-06-15T10:00"
            }

    result <- runExceptT $ action userModel badForm

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"
