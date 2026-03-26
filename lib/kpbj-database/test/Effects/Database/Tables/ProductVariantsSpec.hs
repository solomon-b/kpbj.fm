module Effects.Database.Tables.ProductVariantsSpec where

--------------------------------------------------------------------------------

import Data.Maybe (isJust)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ProductVariants qualified as UUT
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Gen.Tables.ProductVariants (productVariantInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

dummyProductId :: Products.Id
dummyProductId = Products.Id 0

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ProductVariants" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect

      describe "Queries" $ do
        runs 10 . it "getByProductId returns active variants only" $
          hedgehog . prop_getByProductIdFiltersDeleted
        runs 10 . it "getByProductId returns variants ordered by sort_order" $
          hedgehog . prop_getByProductIdOrdered

      describe "Mutations" $ do
        runs 10 . it "softDeleteVariant hides from getByProductId" $
          hedgehog . prop_softDeleteVariant
        runs 10 . it "softDeleteVariant: getById still returns the variant" $
          hedgehog . prop_softDeleteVariantStillExists

--------------------------------------------------------------------------------
-- Helpers

assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.viProductId insert === UUT.pvProductId model
  UUT.viLabel insert === UUT.pvLabel model
  UUT.viPriceCents insert === UUT.pvPriceCents model
  UUT.viInventoryCount insert === UUT.pvInventoryCount model
  UUT.viSku insert === UUT.pvSku model
  UUT.viWeightOz insert === UUT.pvWeightOz model
  UUT.viSortOrder insert === UUT.pvSortOrder model

--------------------------------------------------------------------------------
-- Lens Laws

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let varInsert = varTemplate {UUT.viProductId = productId}
        variantId <- unwrapInsert (UUT.insertVariant varInsert)
        selected <- TRX.statement () (UUT.getById variantId)
        TRX.condemn
        pure (variantId, varInsert, selected)

      assert $ do
        (variantId, varInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch varInsert selected
        UUT.pvId selected === variantId
        UUT.pvDeletedAt selected === Nothing

prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    original <- forAllT (productVariantInsertGen dummyProductId)
    updated <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (UUT.insertVariant original {UUT.viProductId = productId})
        _ <- TRX.statement () (UUT.updateVariant variantId updated {UUT.viProductId = productId})
        selected <- TRX.statement () (UUT.getById variantId)
        TRX.condemn
        pure (variantId, updated {UUT.viProductId = productId}, selected)

      assert $ do
        (variantId, updatedInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updatedInsert selected
        UUT.pvId selected === variantId

--------------------------------------------------------------------------------
-- Query tests

prop_getByProductIdFiltersDeleted :: TestDBConfig -> PropertyT IO ()
prop_getByProductIdFiltersDeleted cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    v1Template <- forAllT (productVariantInsertGen dummyProductId)
    v2Template <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let v1 = v1Template {UUT.viProductId = productId, UUT.viSortOrder = 0}
        let v2 = v2Template {UUT.viProductId = productId, UUT.viSortOrder = 1}
        _v1Id <- unwrapInsert (UUT.insertVariant v1)
        v2Id <- unwrapInsert (UUT.insertVariant v2)

        _ <- TRX.statement () (UUT.softDeleteVariant v2Id)

        active <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure active

      assert $ do
        active <- assertRight result
        length active === 1

prop_getByProductIdOrdered :: TestDBConfig -> PropertyT IO ()
prop_getByProductIdOrdered cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    v1Template <- forAllT (productVariantInsertGen dummyProductId)
    v2Template <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let v1 = v1Template {UUT.viProductId = productId, UUT.viSortOrder = 2}
        let v2 = v2Template {UUT.viProductId = productId, UUT.viSortOrder = 1}
        idA <- unwrapInsert (UUT.insertVariant v1)
        idB <- unwrapInsert (UUT.insertVariant v2)

        variants <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure (idA, idB, variants)

      assert $ do
        (idA, idB, variants) <- assertRight result
        length variants === 2
        map UUT.pvId variants === [idB, idA]

--------------------------------------------------------------------------------
-- Mutation tests

prop_softDeleteVariant :: TestDBConfig -> PropertyT IO ()
prop_softDeleteVariant cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (UUT.insertVariant varTemplate {UUT.viProductId = productId})

        _ <- TRX.statement () (UUT.softDeleteVariant variantId)

        active <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure active

      assert $ do
        active <- assertRight result
        length active === 0

prop_softDeleteVariantStillExists :: TestDBConfig -> PropertyT IO ()
prop_softDeleteVariantStillExists cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (UUT.insertVariant varTemplate {UUT.viProductId = productId})

        _ <- TRX.statement () (UUT.softDeleteVariant variantId)
        selected <- TRX.statement () (UUT.getById variantId)
        TRX.condemn
        pure (variantId, selected)

      assert $ do
        (variantId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.pvId selected === variantId
        -- deleted_at should be set
        Hedgehog.assert (isJust (UUT.pvDeletedAt selected))
