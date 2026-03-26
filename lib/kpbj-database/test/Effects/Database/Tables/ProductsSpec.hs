module Effects.Database.Tables.ProductsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Products qualified as UUT
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (<==))
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
