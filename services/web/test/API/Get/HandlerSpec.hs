module API.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Get.Handler (HomeViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Get.Handler" $ do
      describe "action" $ do
        it "succeeds on an empty database" test_returnsRight
        it "returns Nothing for featured event on empty database" test_noFeaturedEvent
        it "returns the featured event when one is marked" test_featuredEventReturned

--------------------------------------------------------------------------------

-- | Construct a minimal featured event insert for a given author.
mkFeaturedEventInsert :: User.Id -> Events.Insert
mkFeaturedEventInsert userId =
  Events.Insert
    { Events.eiTitle = "Test Featured Event",
      Events.eiSlug = Slug "test-featured-event",
      Events.eiDescription = "A featured event for home page testing.",
      Events.eiStartsAt = read "2099-06-01 10:00:00 UTC",
      Events.eiEndsAt = read "2099-06-01 12:00:00 UTC",
      Events.eiLocationName = "Test Venue",
      Events.eiLocationAddress = "123 Test St",
      Events.eiStatus = Events.Published,
      Events.eiAuthorId = userId,
      Events.eiPosterImageUrl = Nothing,
      Events.eiFeaturedOnHomepage = True
    }

--------------------------------------------------------------------------------

-- | Action always returns Right on an empty database.
test_returnsRight :: TestDBConfig -> IO ()
test_returnsRight cfg = bracketAppM cfg $ do
  result <- runExceptT action
  liftIO $ isRight result `shouldBe` True

-- | Empty database yields Nothing for the featured event.
test_noFeaturedEvent :: TestDBConfig -> IO ()
test_noFeaturedEvent cfg = bracketAppM cfg $ do
  result <- runExceptT action
  liftIO $ case result of
    Left err ->
      expectationFailure $ "Expected Right but got Left: " <> show err
    Right vd ->
      hvdFeaturedEvent vd `shouldBe` Nothing

-- | Inserting an event with eiFeaturedOnHomepage = True and Published status
-- causes the action to return that event as the featured event.
test_featuredEventReturned :: TestDBConfig -> IO ()
test_featuredEventReturned cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "home-test" UserMetadata.User

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    insertTestEvent (mkFeaturedEventInsert userId)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right eventId -> do
      result <- runExceptT action
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd ->
          case hvdFeaturedEvent vd of
            Nothing ->
              expectationFailure "Expected a featured event but got Nothing"
            Just event ->
              Events.emId event `shouldBe` eventId
