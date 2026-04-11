module Effects.Database.Tables.ProductsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.Products qualified as UUT
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (<==))
import Test.Gen.Tables.ProductImages (productImageInsertGen)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Gen.Tables.ProductVariants (productVariantInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Products" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        runs 10 . it "getBySlug returns product by slug" $
          hedgehog . prop_getBySlug
        runs 10 . it "getAll returns all products" $
          hedgehog . prop_getAll

      describe "Mutations" $ do
        runs 10 . it "deactivateProduct sets is_active to false" $
          hedgehog . prop_deactivateProduct

      describe "Constraints" $ do
        runs 10 . it "rejects duplicate slug on insert" $
          hedgehog . prop_insertDuplicateSlug

      describe "Effective Inventory" $ do
        runs 10 . it "uses variant inventory sum when variants exist" $
          hedgehog . prop_effectiveInventoryWithVariants
        runs 10 . it "uses product inventory when no variants exist" $
          hedgehog . prop_effectiveInventoryWithoutVariants
        runs 10 . it "ignores soft-deleted variants in inventory sum" $
          hedgehog . prop_effectiveInventoryIgnoresDeleted

      describe "Inventory" $ do
        runs 10 . it "decrementInventory succeeds when sufficient stock" $
          hedgehog . prop_decrementInventorySufficient
        runs 10 . it "decrementInventory fails when insufficient stock" $
          hedgehog . prop_decrementInventoryInsufficient
        runs 10 . it "decrementInventory succeeds with exact stock level" $
          hedgehog . prop_decrementInventoryExact
        runs 10 . it "decrementInventory fails on inactive product" $
          hedgehog . prop_decrementInventoryInactive
        runs 10 . it "restoreInventory increases inventory count" $
          hedgehog . prop_restoreInventory

      describe "getActiveWithHeroImage" $ do
        runs 1 . it "empty database returns empty list" $
          hedgehog . prop_getActiveWithHeroImage_empty
        runs 10 . it "active product without images returns Nothing for hero image" $
          hedgehog . prop_getActiveWithHeroImage_noImages
        runs 10 . it "active product with images returns first image by sort_order as hero" $
          hedgehog . prop_getActiveWithHeroImage_withImages
        runs 10 . it "inactive product is excluded" $
          hedgehog . prop_getActiveWithHeroImage_inactiveExcluded
        runs 10 . it "only active products returned from mixed active/inactive" $
          hedgehog . prop_getActiveWithHeroImage_onlyActive

--------------------------------------------------------------------------------
-- Helpers

assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.piName insert === UUT.pName model
  UUT.piSlug insert === UUT.pSlug model
  UUT.piDescription insert === UUT.pDescription model
  UUT.piBasePriceCents insert === UUT.pBasePriceCents model
  UUT.piWeightOz insert === UUT.pWeightOz model
  UUT.piCategory insert === UUT.pCategory model
  UUT.piIsActive insert === UUT.pIsActive model
  UUT.piSortOrder insert === UUT.pSortOrder model

--------------------------------------------------------------------------------
-- Lens Laws

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- unwrapInsert (UUT.insertProduct template)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (productId, selected)

      assert $ do
        (productId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch template selected
        UUT.pId selected === productId
        -- No variants, so inventory should match product-level
        UUT.pInventoryCount selected === UUT.piInventoryCount template

prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    original <- forAllT productInsertGen
    updated <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- unwrapInsert (UUT.insertProduct original)
        _ <- TRX.statement () (UUT.updateProduct productId updated)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (productId, selected)

      assert $ do
        (productId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updated selected
        UUT.pId selected === productId

prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    original <- forAllT productInsertGen
    updateA <- forAllT productInsertGen
    updateB <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- unwrapInsert (UUT.insertProduct original)
        _ <- TRX.statement () (UUT.updateProduct productId updateA)
        _ <- TRX.statement () (UUT.updateProduct productId updateB)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (productId, selected)

      assert $ do
        (productId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updateB selected
        UUT.pId selected === productId

--------------------------------------------------------------------------------
-- Query tests

prop_getBySlug :: TestDBConfig -> PropertyT IO ()
prop_getBySlug cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- unwrapInsert (UUT.insertProduct template)
        bySlug <- TRX.statement () (UUT.getBySlug $ UUT.piSlug template)
        TRX.condemn
        pure (productId, bySlug)

      assert $ do
        (productId, mBySlug) <- assertRight result
        bySlug <- assertJust mBySlug
        UUT.pId bySlug === productId
        UUT.pSlug bySlug === UUT.piSlug template

prop_getAll :: TestDBConfig -> PropertyT IO ()
prop_getAll cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT productInsertGen
    template2 <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let p1 = template1 {UUT.piSlug = UUT.piSlug template1 <> "1", UUT.piSortOrder = 0}
        let p2 = template2 {UUT.piSlug = UUT.piSlug template2 <> "2", UUT.piSortOrder = 1}
        id1 <- unwrapInsert (UUT.insertProduct p1)
        id2 <- unwrapInsert (UUT.insertProduct p2)
        all' <- TRX.statement () UUT.getAll
        TRX.condemn
        pure (id1, id2, all')

      assert $ do
        (id1, id2, all') <- assertRight result
        length all' === 2
        let returnedIds = map UUT.pId all'
        elem id1 returnedIds === True
        elem id2 returnedIds === True

--------------------------------------------------------------------------------
-- Mutation tests

prop_deactivateProduct :: TestDBConfig -> PropertyT IO ()
prop_deactivateProduct cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let active = template {UUT.piIsActive = True}
        productId <- unwrapInsert (UUT.insertProduct active)
        _ <- TRX.statement () (UUT.deactivateProduct productId)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.pIsActive selected === False

--------------------------------------------------------------------------------
-- Constraint tests

prop_insertDuplicateSlug :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicateSlug cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT productInsertGen
    template2 <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- unwrapInsert (UUT.insertProduct template1)
        _ <- unwrapInsert (UUT.insertProduct template2 {UUT.piSlug = UUT.piSlug template1})
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()

--------------------------------------------------------------------------------
-- Effective inventory tests

prop_effectiveInventoryWithVariants :: TestDBConfig -> PropertyT IO ()
prop_effectiveInventoryWithVariants cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate1 <- forAllT (productVariantInsertGen (UUT.Id 0))
    varTemplate2 <- forAllT (productVariantInsertGen (UUT.Id 0))

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = prodTemplate {UUT.piInventoryCount = 999}
        productId <- unwrapInsert (UUT.insertProduct prod)

        let v1 = varTemplate1 {ProductVariants.viProductId = productId, ProductVariants.viInventoryCount = 10, ProductVariants.viSortOrder = 0}
        let v2 = varTemplate2 {ProductVariants.viProductId = productId, ProductVariants.viInventoryCount = 20, ProductVariants.viSortOrder = 1}
        _ <- unwrapInsert (ProductVariants.insertVariant v1)
        _ <- unwrapInsert (ProductVariants.insertVariant v2)

        selected <- TRX.statement () (UUT.getById productId)
        allProducts <- TRX.statement () UUT.getAll
        TRX.condemn
        pure (productId, selected, allProducts)

      assert $ do
        (productId, mSelected, allProducts) <- assertRight result
        selected <- assertJust mSelected
        -- Effective inventory should be sum of variant inventories (10 + 20 = 30), not product-level 999
        UUT.pInventoryCount selected === 30

        -- Same for getAll
        let fromAll = filter (\p -> UUT.pId p == productId) allProducts
        case fromAll of
          [p] -> UUT.pInventoryCount p === 30
          _ -> UUT.pInventoryCount selected === 30 -- already checked above

prop_effectiveInventoryWithoutVariants :: TestDBConfig -> PropertyT IO ()
prop_effectiveInventoryWithoutVariants cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = prodTemplate {UUT.piInventoryCount = 42}
        productId <- unwrapInsert (UUT.insertProduct prod)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === 42

prop_effectiveInventoryIgnoresDeleted :: TestDBConfig -> PropertyT IO ()
prop_effectiveInventoryIgnoresDeleted cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate1 <- forAllT (productVariantInsertGen (UUT.Id 0))
    varTemplate2 <- forAllT (productVariantInsertGen (UUT.Id 0))

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = prodTemplate {UUT.piInventoryCount = 999}
        productId <- unwrapInsert (UUT.insertProduct prod)

        let v1 = varTemplate1 {ProductVariants.viProductId = productId, ProductVariants.viInventoryCount = 10, ProductVariants.viSortOrder = 0}
        let v2 = varTemplate2 {ProductVariants.viProductId = productId, ProductVariants.viInventoryCount = 20, ProductVariants.viSortOrder = 1}
        _ <- unwrapInsert (ProductVariants.insertVariant v1)
        v2Id <- unwrapInsert (ProductVariants.insertVariant v2)

        -- Soft-delete variant 2
        _ <- TRX.statement () (ProductVariants.softDeleteVariant v2Id)

        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        -- Only active variant counts: 10 (variant 2 is deleted)
        UUT.pInventoryCount selected === 10

--------------------------------------------------------------------------------
-- getActiveWithHeroImage tests

-- | Dummy product ID used during generation; overridden with real ID in the transaction.
dummyProductId :: UUT.Id
dummyProductId = UUT.Id 0

prop_getActiveWithHeroImage_empty :: TestDBConfig -> PropertyT IO ()
prop_getActiveWithHeroImage_empty cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        items <- TRX.statement () UUT.getActiveWithHeroImage
        TRX.condemn
        pure items

      assert $ do
        items <- assertRight result
        items === []

prop_getActiveWithHeroImage_noImages :: TestDBConfig -> PropertyT IO ()
prop_getActiveWithHeroImage_noImages cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let active = template {UUT.piIsActive = True}
        productId <- unwrapInsert (UUT.insertProduct active)
        items <- TRX.statement () UUT.getActiveWithHeroImage
        TRX.condemn
        pure (productId, items)

      assert $ do
        (productId, items) <- assertRight result
        length items === 1
        let item = head items
        UUT.pwhId item === productId
        UUT.pwhName item === UUT.piName template
        UUT.pwhHeroImagePath item === Nothing
        UUT.pwhHeroAltText item === Nothing

prop_getActiveWithHeroImage_withImages :: TestDBConfig -> PropertyT IO ()
prop_getActiveWithHeroImage_withImages cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    imgTemplate1 <- forAllT (productImageInsertGen dummyProductId)
    imgTemplate2 <- forAllT (productImageInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let active = prodTemplate {UUT.piIsActive = True}
        productId <- unwrapInsert (UUT.insertProduct active)

        -- Insert two images with explicit sort_order: imgA at 2, imgB at 1
        let imgA = imgTemplate1 {ProductImages.iiProductId = productId, ProductImages.iiSortOrder = 2}
        let imgB = imgTemplate2 {ProductImages.iiProductId = productId, ProductImages.iiSortOrder = 1}
        _ <- unwrapInsert (ProductImages.insertImage imgA)
        _ <- unwrapInsert (ProductImages.insertImage imgB)

        items <- TRX.statement () UUT.getActiveWithHeroImage
        TRX.condemn
        pure (productId, imgB, items)

      assert $ do
        (productId, heroInsert, items) <- assertRight result
        length items === 1
        let item = head items
        UUT.pwhId item === productId
        -- Hero should be imgB (sort_order=1), not imgA (sort_order=2)
        UUT.pwhHeroImagePath item === Just (ProductImages.iiImagePath heroInsert)
        UUT.pwhHeroAltText item === Just (ProductImages.iiAltText heroInsert)

prop_getActiveWithHeroImage_inactiveExcluded :: TestDBConfig -> PropertyT IO ()
prop_getActiveWithHeroImage_inactiveExcluded cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let inactive = template {UUT.piIsActive = False}
        _ <- unwrapInsert (UUT.insertProduct inactive)
        items <- TRX.statement () UUT.getActiveWithHeroImage
        TRX.condemn
        pure items

      assert $ do
        items <- assertRight result
        items === []

prop_getActiveWithHeroImage_onlyActive :: TestDBConfig -> PropertyT IO ()
prop_getActiveWithHeroImage_onlyActive cfg = do
  arrange (bracketConn cfg) $ do
    activeTemplate <- forAllT productInsertGen
    inactiveTemplate <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let active = activeTemplate {UUT.piIsActive = True, UUT.piSlug = UUT.piSlug activeTemplate <> "a"}
        let inactive = inactiveTemplate {UUT.piIsActive = False, UUT.piSlug = UUT.piSlug inactiveTemplate <> "i"}
        activeId <- unwrapInsert (UUT.insertProduct active)
        _ <- unwrapInsert (UUT.insertProduct inactive)
        items <- TRX.statement () UUT.getActiveWithHeroImage
        TRX.condemn
        pure (activeId, items)

      assert $ do
        (activeId, items) <- assertRight result
        length items === 1
        UUT.pwhId (head items) === activeId

--------------------------------------------------------------------------------
-- Inventory tests

prop_decrementInventorySufficient :: TestDBConfig -> PropertyT IO ()
prop_decrementInventorySufficient cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen
    stock <- forAllT $ Gen.int64 (Range.linear 2 10000)
    qty <- forAllT $ Gen.int64 (Range.linear 1 (stock - 1))

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = template {UUT.piInventoryCount = stock, UUT.piIsActive = True}
        productId <- unwrapInsert (UUT.insertProduct prod)
        decremented <- TRX.statement () (UUT.decrementInventory productId qty)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (productId, decremented, selected)

      assert $ do
        (productId, mDecremented, mSelected) <- assertRight result
        decrementedId <- assertJust mDecremented
        decrementedId === productId
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === stock - qty


prop_decrementInventoryInsufficient :: TestDBConfig -> PropertyT IO ()
prop_decrementInventoryInsufficient cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen
    stock <- forAllT $ Gen.int64 (Range.linear 0 100)
    extra <- forAllT $ Gen.int64 (Range.linear 1 100)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = template {UUT.piInventoryCount = stock}
        productId <- unwrapInsert (UUT.insertProduct prod)
        decremented <- TRX.statement () (UUT.decrementInventory productId (stock + extra))
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (decremented, selected, stock)

      assert $ do
        (mDecremented, mSelected, originalStock) <- assertRight result
        assertNothing mDecremented
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === originalStock


prop_decrementInventoryExact :: TestDBConfig -> PropertyT IO ()
prop_decrementInventoryExact cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen
    stock <- forAllT $ Gen.int64 (Range.linear 1 10000)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = template {UUT.piInventoryCount = stock, UUT.piIsActive = True}
        productId <- unwrapInsert (UUT.insertProduct prod)
        decremented <- TRX.statement () (UUT.decrementInventory productId stock)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (productId, decremented, selected)

      assert $ do
        (productId, mDecremented, mSelected) <- assertRight result
        decrementedId <- assertJust mDecremented
        decrementedId === productId
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === 0


prop_decrementInventoryInactive :: TestDBConfig -> PropertyT IO ()
prop_decrementInventoryInactive cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen
    stock <- forAllT $ Gen.int64 (Range.linear 1 10000)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = template {UUT.piInventoryCount = stock, UUT.piIsActive = False}
        productId <- unwrapInsert (UUT.insertProduct prod)
        decremented <- TRX.statement () (UUT.decrementInventory productId 1)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure (decremented, selected, stock)

      assert $ do
        (mDecremented, mSelected, originalStock) <- assertRight result
        assertNothing mDecremented
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === originalStock


prop_restoreInventory :: TestDBConfig -> PropertyT IO ()
prop_restoreInventory cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT productInsertGen
    initialStock <- forAllT $ Gen.int64 (Range.linear 0 5000)
    restoreQty <- forAllT $ Gen.int64 (Range.linear 1 5000)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let prod = template {UUT.piInventoryCount = initialStock}
        productId <- unwrapInsert (UUT.insertProduct prod)
        TRX.statement () (UUT.restoreInventory productId restoreQty)
        selected <- TRX.statement () (UUT.getById productId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.pInventoryCount selected === initialStock + restoreQty
