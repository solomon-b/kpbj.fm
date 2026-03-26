module Effects.Database.Tables.ProductImagesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ProductImages qualified as UUT
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.ProductImages (productImageInsertGen)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- | Dummy product ID used during generation; overridden with real ID in the transaction.
dummyProductId :: Products.Id
dummyProductId = Products.Id 0

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ProductImages" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        runs 10 . it "getByProductId returns images for correct product" $
          hedgehog . prop_getByProductId
        runs 10 . it "getByProductId returns images ordered by sort_order" $
          hedgehog . prop_getByProductIdOrdered

      describe "Mutations" $ do
        runs 10 . it "updateImageMeta updates sort_order and alt_text" $
          hedgehog . prop_updateImageMeta
        runs 10 . it "deleteImage removes image" $
          hedgehog . prop_deleteImage

--------------------------------------------------------------------------------
-- Helpers

assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.iiProductId insert === UUT.piProductId model
  UUT.iiImagePath insert === UUT.piImagePath model
  UUT.iiAltText insert === UUT.piAltText model
  UUT.iiSortOrder insert === UUT.piSortOrder model

--------------------------------------------------------------------------------
-- Lens Laws

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    imgTemplate <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let imgInsert = imgTemplate {UUT.iiProductId = productId}
        imageId <- unwrapInsert (UUT.insertImage imgInsert)
        selected <- TRX.statement () (UUT.getById imageId)
        TRX.condemn
        pure (imageId, imgInsert, selected)

      assert $ do
        (imageId, imgInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch imgInsert selected
        UUT.piId selected === imageId

--------------------------------------------------------------------------------
-- Query tests

prop_getByProductId :: TestDBConfig -> PropertyT IO ()
prop_getByProductId cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate1 <- forAllT productInsertGen
    prodTemplate2 <- forAllT productInsertGen
    imgTemplate1 <- forAllT (productImageInsertGen dummyProductId)
    imgTemplate2 <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let p1 = prodTemplate1 {Products.piSlug = Products.piSlug prodTemplate1 <> "1"}
        let p2 = prodTemplate2 {Products.piSlug = Products.piSlug prodTemplate2 <> "2"}
        pid1 <- insertTestProduct p1
        pid2 <- insertTestProduct p2

        _ <- unwrapInsert (UUT.insertImage imgTemplate1 {UUT.iiProductId = pid1})
        _ <- unwrapInsert (UUT.insertImage imgTemplate2 {UUT.iiProductId = pid2})

        imagesForP1 <- TRX.statement () (UUT.getByProductId pid1)
        imagesForP2 <- TRX.statement () (UUT.getByProductId pid2)
        TRX.condemn
        pure (imagesForP1, imagesForP2)

      assert $ do
        (imagesForP1, imagesForP2) <- assertRight result
        length imagesForP1 === 1
        length imagesForP2 === 1

prop_getByProductIdOrdered :: TestDBConfig -> PropertyT IO ()
prop_getByProductIdOrdered cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    imgTemplate1 <- forAllT (productImageInsertGen dummyProductId)
    imgTemplate2 <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate

        let imgA = imgTemplate1 {UUT.iiProductId = productId, UUT.iiSortOrder = 2}
        let imgB = imgTemplate2 {UUT.iiProductId = productId, UUT.iiSortOrder = 1}
        idA <- unwrapInsert (UUT.insertImage imgA)
        idB <- unwrapInsert (UUT.insertImage imgB)

        images <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure (idA, idB, images)

      assert $ do
        (idA, idB, images) <- assertRight result
        length images === 2
        -- B (sort_order=1) should come before A (sort_order=2)
        map UUT.piId images === [idB, idA]

--------------------------------------------------------------------------------
-- Mutation tests

prop_updateImageMeta :: TestDBConfig -> PropertyT IO ()
prop_updateImageMeta cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    imgTemplate <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        imageId <- unwrapInsert (UUT.insertImage imgTemplate {UUT.iiProductId = productId})

        TRX.statement () (UUT.updateImageMeta imageId 42 "new alt text")
        selected <- TRX.statement () (UUT.getById imageId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.piSortOrder selected === 42
        UUT.piAltText selected === "new alt text"

prop_deleteImage :: TestDBConfig -> PropertyT IO ()
prop_deleteImage cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    imgTemplate <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        imageId <- unwrapInsert (UUT.insertImage imgTemplate {UUT.iiProductId = productId})

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
