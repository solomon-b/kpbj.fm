module Effects.Database.Tables.ProductOptionTypesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ProductOptionTypes qualified as UUT
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, assertSingleton)
import Test.Gen.Tables.ProductOptionTypes (productOptionTypeInsertGen)
import Test.Gen.Tables.ProductOptionValues (productOptionValueInsertGen)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

dummyProductId :: Products.Id
dummyProductId = Products.Id 0

dummyOptionTypeId :: UUT.Id
dummyOptionTypeId = UUT.Id 0

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ProductOptionTypes" $ do
      describe "Queries" $ do
        runs 10 . it "insert and getByProductId round-trip" $
          hedgehog . prop_insertAndGet
        runs 10 . it "getByProductId returns options ordered by sort_order" $
          hedgehog . prop_getByProductIdOrdered

      describe "Cascade Delete" $ do
        runs 10 . it "deleteByProductId cascades to option values" $
          hedgehog . prop_deleteByProductIdCascades
        runs 10 . it "deleteOptionType cascades to option values" $
          hedgehog . prop_deleteOptionTypeCascades

--------------------------------------------------------------------------------

prop_insertAndGet :: TestDBConfig -> PropertyT IO ()
prop_insertAndGet cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let optInsert = optTemplate {UUT.otiProductId = productId}
        optId <- TRX.statement () (UUT.insertOptionType optInsert)
        options <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure (optId, optInsert, options)

      assert $ do
        (optId, optInsert, options) <- assertRight result
        opt <- assertSingleton options
        UUT.potId opt === optId
        UUT.potName opt === UUT.otiName optInsert
        UUT.potSortOrder opt === UUT.otiSortOrder optInsert

prop_getByProductIdOrdered :: TestDBConfig -> PropertyT IO ()
prop_getByProductIdOrdered cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    opt1 <- forAllT (productOptionTypeInsertGen dummyProductId)
    opt2 <- forAllT (productOptionTypeInsertGen dummyProductId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        let o1 = opt1 {UUT.otiProductId = productId, UUT.otiSortOrder = 2}
        let o2 = opt2 {UUT.otiProductId = productId, UUT.otiSortOrder = 1}
        idA <- TRX.statement () (UUT.insertOptionType o1)
        idB <- TRX.statement () (UUT.insertOptionType o2)
        options <- TRX.statement () (UUT.getByProductId productId)
        TRX.condemn
        pure (idA, idB, options)

      assert $ do
        (idA, idB, options) <- assertRight result
        length options === 2
        map UUT.potId options === [idB, idA]

prop_deleteByProductIdCascades :: TestDBConfig -> PropertyT IO ()
prop_deleteByProductIdCascades cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    valTemplate <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        optId <- TRX.statement () (UUT.insertOptionType optTemplate {UUT.otiProductId = productId})

        _ <- TRX.statement () (ProductOptionValues.insertOptionValue valTemplate {ProductOptionValues.oviOptionTypeId = optId})

        _ <- TRX.statement () (UUT.deleteByProductId productId)

        optionsAfter <- TRX.statement () (UUT.getByProductId productId)
        valuesAfter <- TRX.statement () (ProductOptionValues.getByOptionTypeId optId)
        TRX.condemn
        pure (optionsAfter, valuesAfter)

      assert $ do
        (optionsAfter, valuesAfter) <- assertRight result
        length optionsAfter === 0
        length valuesAfter === 0

prop_deleteOptionTypeCascades :: TestDBConfig -> PropertyT IO ()
prop_deleteOptionTypeCascades cfg = do
  arrange (bracketConn cfg) $ do
    prodTemplate <- forAllT productInsertGen
    optTemplate <- forAllT (productOptionTypeInsertGen dummyProductId)
    valTemplate <- forAllT (productOptionValueInsertGen dummyOptionTypeId)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct prodTemplate
        optId <- TRX.statement () (UUT.insertOptionType optTemplate {UUT.otiProductId = productId})

        _ <- TRX.statement () (ProductOptionValues.insertOptionValue valTemplate {ProductOptionValues.oviOptionTypeId = optId})

        _ <- TRX.statement () (UUT.deleteOptionType optId)

        valuesAfter <- TRX.statement () (ProductOptionValues.getByOptionTypeId optId)
        TRX.condemn
        pure valuesAfter

      assert $ do
        valuesAfter <- assertRight result
        length valuesAfter === 0
