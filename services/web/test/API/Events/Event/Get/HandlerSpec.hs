module API.Events.Event.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Handler (EventViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text qualified as Text
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Events.Event.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "returns EventContent when the slug matches" test_returnsContentWithCorrectSlug
        it "returns EventRedirect when the slug does not match" test_redirectsWithWrongSlug

--------------------------------------------------------------------------------

-- | Construct a minimal event insert for a given author and slug.
mkEventInsert :: User.Id -> Slug -> Events.Insert
mkEventInsert userId slug =
  Events.Insert
    { Events.eiTitle = "Test Event",
      Events.eiSlug = slug,
      Events.eiDescription = "A test event for detail page testing.",
      Events.eiStartsAt = read "2099-07-01 10:00:00 UTC",
      Events.eiEndsAt = read "2099-07-01 12:00:00 UTC",
      Events.eiLocationName = "Test Venue",
      Events.eiLocationAddress = "123 Test St",
      Events.eiStatus = Events.Published,
      Events.eiAuthorId = userId,
      Events.eiPosterImageUrl = Nothing,
      Events.eiFeaturedOnHomepage = False
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID returns a NotFound error.
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg = bracketAppM cfg $ do
  result <- runExceptT $ action (Events.Id nonExistentId) (Just (Slug "any-slug"))
  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err ->
      expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ ->
      expectationFailure "Expected Left NotFound but got Right"

-- | When the URL slug matches the event's canonical slug, EventContent is returned.
test_returnsContentWithCorrectSlug :: TestDBConfig -> IO ()
test_returnsContentWithCorrectSlug cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "event-detail-test" UserMetadata.User
  let canonicalSlug = Slug "correct-slug-event"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    insertTestEvent (mkEventInsert userId canonicalSlug)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right eventId -> do
      result <- runExceptT $ action eventId (Just canonicalSlug)
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right (EventRedirect url) ->
          expectationFailure $ "Expected EventContent but got EventRedirect to: " <> show url
        Right (EventContent _backend event) ->
          Events.emId event `shouldBe` eventId

-- | When the URL slug does not match the event's canonical slug, EventRedirect
-- is returned with a URL containing the correct canonical slug.
test_redirectsWithWrongSlug :: TestDBConfig -> IO ()
test_redirectsWithWrongSlug cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "event-detail-test" UserMetadata.User
  -- Insert the event with a well-known canonical slug.
  let canonicalSlug = Slug "canonical-slug"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    insertTestEvent (mkEventInsert userId canonicalSlug)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right eventId -> do
      -- Supply a URL slug that differs from the canonical one.
      let wrongSlug = Just (Slug "wrong-slug")
      result <- runExceptT $ action eventId wrongSlug
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right (EventContent _ _) ->
          expectationFailure "Expected EventRedirect but got EventContent"
        Right (EventRedirect url) -> do
          -- The redirect URL must contain the canonical slug, not the wrong one.
          url `shouldSatisfy` Text.isInfixOf "canonical-slug"
          url `shouldSatisfy` (not . Text.isInfixOf "wrong-slug")
