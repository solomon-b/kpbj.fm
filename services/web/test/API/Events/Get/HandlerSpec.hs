module API.Events.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Events.Get.Handler (EventsViewData (..), action)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.Events (eventInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Events.Get.Handler" $ do
      describe "action" $ do
        runs 1 . it "returns empty events list on empty database" $
          hedgehog . prop_emptyDB

        runs 5 . it "always returns Right on a valid database" $
          hedgehog . prop_returnsRight

        runs 5 . it "returns inserted published events" $
          hedgehog . prop_returnsInsertedEvents

--------------------------------------------------------------------------------

-- | Empty database returns an empty events list.
prop_emptyDB :: TestDBConfig -> PropertyT IO ()
prop_emptyDB cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT action
      assert $ do
        vd <- assertRight result
        evdEvents vd === []

-- | Action always succeeds (returns Right) on a valid database.
prop_returnsRight :: TestDBConfig -> PropertyT IO ()
prop_returnsRight cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT action
      assert $ do
        H.assert (isRight result)

-- | Inserting N published events causes them to all appear in the result.
--
-- The event generator produces random statuses, so we override eiStatus to
-- Published to ensure they are returned by getPublishedEvents.
prop_returnsInsertedEvents :: TestDBConfig -> PropertyT IO ()
prop_returnsInsertedEvents cfg = do
  arrange (bracketAppM cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    eventInserts <- forAllT $ Gen.list (Range.linear 1 5) (eventInsertGen (User.Id 0))

    act $ do
      -- Count events already in the DB before our inserts.
      beforeResult <- runExceptT action
      let beforeCount = either (const 0) (length . evdEvents) beforeResult

      -- Insert user then override each event's authorId and status.
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        mapM_
          ( \ei ->
              insertTestEvent
                ei
                  { Events.eiAuthorId = userId,
                    Events.eiStatus = Events.Published
                  }
          )
          eventInserts

      afterResult <- runExceptT action

      assert $ do
        vd <- assertRight afterResult
        length (evdEvents vd) === beforeCount + length eventInserts
