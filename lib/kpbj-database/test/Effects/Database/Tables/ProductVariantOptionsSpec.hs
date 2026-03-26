module Effects.Database.Tables.ProductVariantOptionsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.ProductVariantOptions qualified as UUT
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.ProductOptionTypes (productOptionTypeInsertGen)
import Test.Gen.Tables.ProductOptionValues (productOptionValueInsertGen)
import Test.Gen.Tables.ProductVariants (productVariantInsertGen)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

dummyProductId :: Products.Id
dummyProductId = Products.Id 0

dummyOptionTypeId :: ProductOptionTypes.Id
dummyOptionTypeId = ProductOptionTypes.Id 0

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ProductVariantOptions" $ do
      describe "Queries" $ do
        runs 10 . it "insert and getByVariantId round-trip" $
          hedgehog . prop_insertAndGetByVariantId
        runs 10 . it "getByProductId returns rows for active variants only" $
          hedgehog . prop_getByProductIdFiltersDeleted
        runs 10 . it "ON CONFLICT DO NOTHING on duplicate insert" $
          hedgehog . prop_duplicateInsertIsIdempotent

      describe "Mutations" $ do
        runs 10 . it "deleteByVariantId removes all associations for variant" $
          hedgehog . prop_deleteByVariantId

--------------------------------------------------------------------------------

prop_insertAndGetByVariantId :: TestDBConfig -> PropertyT IO ()
prop_insertAndGetByVariantId cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    valTemplate <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (ProductVariants.insertVariant varTemplate {ProductVariants.viProductId = productId})
        optTypeId <- TRX.statement () (ProductOptionTypes.insertOptionType optTemplate {ProductOptionTypes.otiProductId = productId})
        optValueId <- TRX.statement () (ProductOptionValues.insertOptionValue valTemplate {ProductOptionValues.oviOptionTypeId = optTypeId})

        TRX.statement () (UUT.insertVariantOption variantId optValueId)
        rows <- TRX.statement () (UUT.getByVariantId variantId)
        TRX.condemn
        pure (optValueId, rows)

      assert $ do
        (optValueId, rows) <- assertRight result
        length rows === 1
        rows === [optValueId]

prop_getByProductIdFiltersDeleted :: TestDBConfig -> PropertyT IO ()
prop_getByProductIdFiltersDeleted cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    v1Template <- forAllT (productVariantInsertGen dummyProductId)
    v2Template <- forAllT (productVariantInsertGen dummyProductId)
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    val1Template <- forAllT (productOptionValueInsertGen dummyOptionTypeId)
    val2Template <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        v1Id <- unwrapInsert (ProductVariants.insertVariant v1Template {ProductVariants.viProductId = productId, ProductVariants.viSortOrder = 0})
        v2Id <- unwrapInsert (ProductVariants.insertVariant v2Template {ProductVariants.viProductId = productId, ProductVariants.viSortOrder = 1})
        optTypeId <- TRX.statement () (ProductOptionTypes.insertOptionType optTemplate {ProductOptionTypes.otiProductId = productId})
        val1Id <- TRX.statement () (ProductOptionValues.insertOptionValue val1Template {ProductOptionValues.oviOptionTypeId = optTypeId, ProductOptionValues.oviSortOrder = 0})
        val2Id <- TRX.statement () (ProductOptionValues.insertOptionValue val2Template {ProductOptionValues.oviOptionTypeId = optTypeId, ProductOptionValues.oviSortOrder = 1})

        TRX.statement () (UUT.insertVariantOption v1Id val1Id)
        TRX.statement () (UUT.insertVariantOption v2Id val2Id)

        -- Soft-delete v2
        _ <- TRX.statement () (ProductVariants.softDeleteVariant v2Id)

        rows <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure (v1Id, rows)

      assert $ do
        (v1Id, rows) <- assertRight result
        length rows === 1
        map UUT.voVariantId rows === [v1Id]

prop_duplicateInsertIsIdempotent :: TestDBConfig -> PropertyT IO ()
prop_duplicateInsertIsIdempotent cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    valTemplate <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (ProductVariants.insertVariant varTemplate {ProductVariants.viProductId = productId})
        optTypeId <- TRX.statement () (ProductOptionTypes.insertOptionType optTemplate {ProductOptionTypes.otiProductId = productId})
        optValueId <- TRX.statement () (ProductOptionValues.insertOptionValue valTemplate {ProductOptionValues.oviOptionTypeId = optTypeId})

        TRX.statement () (UUT.insertVariantOption variantId optValueId)
        TRX.statement () (UUT.insertVariantOption variantId optValueId) -- duplicate
        rows <- TRX.statement () (UUT.getByVariantId variantId)
        TRX.condemn
        pure rows

      assert $ do
        rows <- assertRight result
        length rows === 1

prop_deleteByVariantId :: TestDBConfig -> PropertyT IO ()
prop_deleteByVariantId cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    varTemplate <- forAllT (productVariantInsertGen dummyProductId)
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    val1Template <- forAllT (productOptionValueInsertGen dummyOptionTypeId)
    val2Template <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        variantId <- unwrapInsert (ProductVariants.insertVariant varTemplate {ProductVariants.viProductId = productId})
        optTypeId <- TRX.statement () (ProductOptionTypes.insertOptionType optTemplate {ProductOptionTypes.otiProductId = productId})
        val1Id <- TRX.statement () (ProductOptionValues.insertOptionValue val1Template {ProductOptionValues.oviOptionTypeId = optTypeId, ProductOptionValues.oviSortOrder = 0})
        val2Id <- TRX.statement () (ProductOptionValues.insertOptionValue val2Template {ProductOptionValues.oviOptionTypeId = optTypeId, ProductOptionValues.oviSortOrder = 1})

        TRX.statement () (UUT.insertVariantOption variantId val1Id)
        TRX.statement () (UUT.insertVariantOption variantId val2Id)

        TRX.statement () (UUT.deleteByVariantId variantId)
        rows <- TRX.statement () (UUT.getByVariantId variantId)
        TRX.condemn
        pure rows

      assert $ do
        rows <- assertRight result
        length rows === 0
