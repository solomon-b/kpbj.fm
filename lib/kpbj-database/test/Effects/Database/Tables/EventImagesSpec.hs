module Effects.Database.Tables.EventImagesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EventImages qualified as UUT
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestEvent, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.EventImages (eventImageInsertGen)
import Test.Gen.Tables.Events (eventInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- | Dummy event ID used during generation; overridden with a real ID in the transaction.
dummyEventId :: Events.Id
dummyEventId = Events.Id 0

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.EventImages" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        runs 10 . it "getByEventId returns images for correct event" $
          hedgehog . prop_getByEventId
        runs 10 . it "getByEventId returns images ordered by sort_order" $
          hedgehog . prop_getByEventIdOrdered

      describe "Mutations" $ do
        runs 10 . it "updateImageMeta updates sort_order, caption, and alt_text" $
          hedgehog . prop_updateImageMeta
        runs 10 . it "deleteImage removes image" $
          hedgehog . prop_deleteImage

--------------------------------------------------------------------------------
-- Helpers

assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.iiEventId insert === UUT.eviEventId model
  UUT.iiImagePath insert === UUT.eviImagePath model
  UUT.iiCaption insert === UUT.eviCaption model
  UUT.iiAltText insert === UUT.eviAltText model
  UUT.iiSortOrder insert === UUT.eviSortOrder model

--------------------------------------------------------------------------------
-- Lens Laws

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT (eventInsertGen (User.Id 1))
    imgTemplate <- forAllT (eventImageInsertGen dummyEventId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        eventId <- insertTestEvent (eventTemplate {Events.eiAuthorId = userId})
        let imgInsert = imgTemplate {UUT.iiEventId = eventId}
        imageId <- unwrapInsert (UUT.insertImage imgInsert)
        selected <- TRX.statement () (UUT.getById imageId)
        TRX.condemn
        pure (imageId, imgInsert, selected)

      assert $ do
        (imageId, imgInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch imgInsert selected
        UUT.eviId selected === imageId

--------------------------------------------------------------------------------
-- Query tests

prop_getByEventId :: TestDBConfig -> PropertyT IO ()
prop_getByEventId cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen
    eventTemplate1 <- forAllT (eventInsertGen (User.Id 1))
    eventTemplate2 <- forAllT (eventInsertGen (User.Id 1))
    imgTemplate1 <- forAllT (eventImageInsertGen dummyEventId)
    imgTemplate2 <- forAllT (eventImageInsertGen dummyEventId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        uid1 <- insertTestUser userWithMetadata1
        uid2 <- insertTestUser userWithMetadata2
        let e1 = eventTemplate1 {Events.eiAuthorId = uid1, Events.eiSlug = Events.eiSlug eventTemplate1 <> "1"}
        let e2 = eventTemplate2 {Events.eiAuthorId = uid2, Events.eiSlug = Events.eiSlug eventTemplate2 <> "2"}
        eid1 <- insertTestEvent e1
        eid2 <- insertTestEvent e2

        _ <- unwrapInsert (UUT.insertImage imgTemplate1 {UUT.iiEventId = eid1})
        _ <- unwrapInsert (UUT.insertImage imgTemplate2 {UUT.iiEventId = eid2})

        imagesForE1 <- TRX.statement () (UUT.getByEventId eid1)
        imagesForE2 <- TRX.statement () (UUT.getByEventId eid2)
        TRX.condemn
        pure (imagesForE1, imagesForE2)

      assert $ do
        (imagesForE1, imagesForE2) <- assertRight result
        length imagesForE1 === 1
        length imagesForE2 === 1

prop_getByEventIdOrdered :: TestDBConfig -> PropertyT IO ()
prop_getByEventIdOrdered cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT (eventInsertGen (User.Id 1))
    imgTemplate1 <- forAllT (eventImageInsertGen dummyEventId)
    imgTemplate2 <- forAllT (eventImageInsertGen dummyEventId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        eventId <- insertTestEvent (eventTemplate {Events.eiAuthorId = userId})

        let imgA = imgTemplate1 {UUT.iiEventId = eventId, UUT.iiSortOrder = 2}
        let imgB = imgTemplate2 {UUT.iiEventId = eventId, UUT.iiSortOrder = 1}
        idA <- unwrapInsert (UUT.insertImage imgA)
        idB <- unwrapInsert (UUT.insertImage imgB)

        images <- TRX.statement () (UUT.getByEventId eventId)
        TRX.condemn
        pure (idA, idB, images)

      assert $ do
        (idA, idB, images) <- assertRight result
        length images === 2
        -- B (sort_order=1) should come before A (sort_order=2)
        map UUT.eviId images === [idB, idA]

--------------------------------------------------------------------------------
-- Mutation tests

prop_updateImageMeta :: TestDBConfig -> PropertyT IO ()
prop_updateImageMeta cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT (eventInsertGen (User.Id 1))
    imgTemplate <- forAllT (eventImageInsertGen dummyEventId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        eventId <- insertTestEvent (eventTemplate {Events.eiAuthorId = userId})
        imageId <- unwrapInsert (UUT.insertImage imgTemplate {UUT.iiEventId = eventId})

        TRX.statement () (UUT.updateImageMeta imageId 42 "new caption" "new alt text")
        selected <- TRX.statement () (UUT.getById imageId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.eviSortOrder selected === 42
        UUT.eviCaption selected === "new caption"
        UUT.eviAltText selected === "new alt text"

prop_deleteImage :: TestDBConfig -> PropertyT IO ()
prop_deleteImage cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT (eventInsertGen (User.Id 1))
    imgTemplate <- forAllT (eventImageInsertGen dummyEventId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        eventId <- insertTestEvent (eventTemplate {Events.eiAuthorId = userId})
        imageId <- unwrapInsert (UUT.insertImage imgTemplate {UUT.iiEventId = eventId})

        deleteResult <- TRX.statement () (UUT.deleteImage imageId)
        afterDelete <- TRX.statement () (UUT.getById imageId)
        TRX.condemn
        pure (imageId, deleteResult, afterDelete)

      assert $ do
        (imageId, deleteResult, afterDelete) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === imageId
        assertNothing afterDelete
        pure ()
