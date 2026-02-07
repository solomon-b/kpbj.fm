module Effects.Database.Tables.EventsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as UUT
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton, (<==))
import Test.Gen.Tables.Events (eventInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Events" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        describe "getPublishedEvents" $ do
          runs 10 . it "filters by Published status" $ hedgehog . prop_getPublishedEvents
        describe "getAllEvents" $ do
          runs 10 . it "returns all events regardless of status" $ hedgehog . prop_getAllEvents
          runs 10 . it "respects Limit" $ hedgehog . prop_getAllEvents_limit
          runs 10 . it "respects Offset" $ hedgehog . prop_getAllEvents_offset

      describe "Mutations" $ do
        runs 10 . it "deleteEvent removes event (hard delete)" $ hedgehog . prop_deleteEvent

      describe "Constraints" $ do
        runs 10 . it "rejects duplicate slug on insert" $ hedgehog . prop_insertDuplicateSlug

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.eiTitle insert === UUT.emTitle model
  UUT.eiSlug insert === UUT.emSlug model
  UUT.eiDescription insert === UUT.emDescription model
  UUT.eiLocationName insert === UUT.emLocationName model
  UUT.eiLocationAddress insert === UUT.emLocationAddress model
  UUT.eiStatus insert === UUT.emStatus model
  UUT.eiAuthorId insert === UUT.emAuthorId model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let eventInsert = eventTemplate {UUT.eiAuthorId = userId}

        eventId <- TRX.statement () (UUT.insertEvent eventInsert)
        selected <- TRX.statement () (UUT.getEventById eventId)
        TRX.condemn
        pure (eventId, eventInsert, selected)

      assert $ do
        (eventId, eventInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch eventInsert selected
        eventId === UUT.emId selected
        pure ()

-- | Update-Select: update then select returns updated values.
prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    originalTemplate <- forAllT $ eventInsertGen (User.Id 1)
    updateTemplate <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let original = originalTemplate {UUT.eiAuthorId = userId}
        eventId <- TRX.statement () (UUT.insertEvent original)

        let updated = updateTemplate {UUT.eiAuthorId = userId}
        updateResult <- TRX.statement () (UUT.updateEvent eventId updated)

        selected <- TRX.statement () (UUT.getEventById eventId)
        TRX.condemn
        pure (eventId, updated, updateResult, selected)

      assert $ do
        (eventId, updated, updateResult, mSelected) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === eventId

        selected <- assertJust mSelected
        assertInsertFieldsMatch updated selected
        UUT.emId selected === eventId
        pure ()

-- | Update-Update: second update fully overwrites first.
prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    originalTemplate <- forAllT $ eventInsertGen (User.Id 1)
    updateATemplate <- forAllT $ eventInsertGen (User.Id 1)
    updateBTemplate <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let original = originalTemplate {UUT.eiAuthorId = userId}
        eventId <- TRX.statement () (UUT.insertEvent original)

        let updateA = updateATemplate {UUT.eiAuthorId = userId}
        _ <- TRX.statement () (UUT.updateEvent eventId updateA)

        let updateB = updateBTemplate {UUT.eiAuthorId = userId}
        _ <- TRX.statement () (UUT.updateEvent eventId updateB)

        selected <- TRX.statement () (UUT.getEventById eventId)
        TRX.condemn
        pure (eventId, updateB, selected)

      assert $ do
        (eventId, updateB, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updateB selected
        UUT.emId selected === eventId
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getPublishedEvents: filters by Published status only.
prop_getPublishedEvents :: TestDBConfig -> PropertyT IO ()
prop_getPublishedEvents cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ eventInsertGen (User.Id 1)
    template2 <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let publishedEvent = template1 {UUT.eiAuthorId = userId, UUT.eiStatus = UUT.Published, UUT.eiSlug = UUT.eiSlug template1 <> Slug "1"}
        let draftEvent = template2 {UUT.eiAuthorId = userId, UUT.eiStatus = UUT.Draft, UUT.eiSlug = UUT.eiSlug template2 <> Slug "2"}

        publishedId <- TRX.statement () (UUT.insertEvent publishedEvent)
        _draftId <- TRX.statement () (UUT.insertEvent draftEvent)

        published <- TRX.statement () (UUT.getPublishedEvents (Limit 10) (Offset 0))
        TRX.condemn
        pure (publishedId, published)

      assert $ do
        (publishedId, published) <- assertRight result
        post <- assertSingleton published
        UUT.emId post === publishedId
        UUT.emStatus post === UUT.Published
        pure ()

-- | getAllEvents: returns all regardless of status.
prop_getAllEvents :: TestDBConfig -> PropertyT IO ()
prop_getAllEvents cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ eventInsertGen (User.Id 1)
    template2 <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let event1 = template1 {UUT.eiAuthorId = userId, UUT.eiStatus = UUT.Published, UUT.eiSlug = UUT.eiSlug template1 <> Slug "1"}
        let event2 = template2 {UUT.eiAuthorId = userId, UUT.eiStatus = UUT.Draft, UUT.eiSlug = UUT.eiSlug template2 <> Slug "2"}

        _ <- TRX.statement () (UUT.insertEvent event1)
        _ <- TRX.statement () (UUT.insertEvent event2)

        allEvents <- TRX.statement () (UUT.getAllEvents (Limit 10) (Offset 0))
        TRX.condemn
        pure allEvents

      assert $ do
        allEvents <- assertRight result
        length allEvents === 2
        pure ()

-- | getAllEvents: respects Limit.
prop_getAllEvents_limit :: TestDBConfig -> PropertyT IO ()
prop_getAllEvents_limit cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ eventInsertGen (User.Id 1)
    template2 <- forAllT $ eventInsertGen (User.Id 1)
    template3 <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let event1 = template1 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template1 <> Slug "1"}
        let event2 = template2 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template2 <> Slug "2"}
        let event3 = template3 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template3 <> Slug "3"}

        _ <- TRX.statement () (UUT.insertEvent event1)
        _ <- TRX.statement () (UUT.insertEvent event2)
        _ <- TRX.statement () (UUT.insertEvent event3)

        limited <- TRX.statement () (UUT.getAllEvents (Limit 2) (Offset 0))
        TRX.condemn
        pure limited

      assert $ do
        limited <- assertRight result
        length limited === 2
        pure ()

-- | getAllEvents: respects Offset.
prop_getAllEvents_offset :: TestDBConfig -> PropertyT IO ()
prop_getAllEvents_offset cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ eventInsertGen (User.Id 1)
    template2 <- forAllT $ eventInsertGen (User.Id 1)
    template3 <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let event1 = template1 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template1 <> Slug "1"}
        let event2 = template2 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template2 <> Slug "2"}
        let event3 = template3 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug template3 <> Slug "3"}

        _ <- TRX.statement () (UUT.insertEvent event1)
        _ <- TRX.statement () (UUT.insertEvent event2)
        _ <- TRX.statement () (UUT.insertEvent event3)

        allEvents <- TRX.statement () (UUT.getAllEvents (Limit 10) (Offset 0))
        offset1 <- TRX.statement () (UUT.getAllEvents (Limit 10) (Offset 1))
        offset2 <- TRX.statement () (UUT.getAllEvents (Limit 10) (Offset 2))
        offset3 <- TRX.statement () (UUT.getAllEvents (Limit 10) (Offset 3))
        TRX.condemn
        pure (allEvents, offset1, offset2, offset3)

      assert $ do
        (allEvents, offset1, offset2, offset3) <- assertRight result
        length allEvents === 3
        length offset1 === 2
        length offset2 === 1
        length offset3 === 0
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteEvent: hard delete, getById returns Nothing.
prop_deleteEvent :: TestDBConfig -> PropertyT IO ()
prop_deleteEvent cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let event = template {UUT.eiAuthorId = userId}
        eventId <- TRX.statement () (UUT.insertEvent event)

        deleteResult <- TRX.statement () (UUT.deleteEvent eventId)
        afterDelete <- TRX.statement () (UUT.getEventById eventId)
        TRX.condemn
        pure (eventId, deleteResult, afterDelete)

      assert $ do
        (eventId, deleteResult, afterDelete) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === eventId
        assertNothing afterDelete
        pure ()

--------------------------------------------------------------------------------
-- Constraint tests

-- | Rejects duplicate slug on insert.
prop_insertDuplicateSlug :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicateSlug cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ eventInsertGen (User.Id 1)
    template2 <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let event1 = template1 {UUT.eiAuthorId = userId}
        _ <- TRX.statement () (UUT.insertEvent event1)

        -- Insert second event with same slug
        let event2 = template2 {UUT.eiAuthorId = userId, UUT.eiSlug = UUT.eiSlug event1}
        _ <- TRX.statement () (UUT.insertEvent event2)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()
