module Effects.Database.Tables.OrderItemsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.OrderItems qualified as UUT
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestProduct, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.OrderItems (orderItemInsertGen)
import Test.Gen.Tables.Orders (orderInsertGen)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.OrderItems" $ do
      describe "Insert and Select" $ do
        runs 10 . it "insertOrderItem-getByOrderId: inserted items retrieved by order" $
          hedgehog . prop_insertGetByOrderId
        runs 10 . it "getByOrderId returns empty list for order with no items" $
          hedgehog . prop_getByOrderIdEmpty
        runs 10 . it "getByOrderId returns items ordered by id" $
          hedgehog . prop_getByOrderIdOrdered

      describe "Fields" $ do
        runs 10 . it "inserted fields match selected fields" $
          hedgehog . prop_insertFieldsMatch

--------------------------------------------------------------------------------
-- Helpers

assertItemFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertItemFieldsMatch insert model = do
  UUT.iiOrderId insert === UUT.oiOrderId model
  UUT.iiProductId insert === UUT.oiProductId model
  UUT.iiVariantId insert === UUT.oiVariantId model
  UUT.iiProductName insert === UUT.oiProductName model
  UUT.iiVariantLabel insert === UUT.oiVariantLabel model
  UUT.iiQuantity insert === UUT.oiQuantity model
  UUT.iiUnitPriceCents insert === UUT.oiUnitPriceCents model

--------------------------------------------------------------------------------
-- Insert and Select

prop_insertGetByOrderId :: TestDBConfig -> PropertyT IO ()
prop_insertGetByOrderId cfg = do
  arrange (bracketConn cfg) $ do
    orderTemplate <- forAllT orderInsertGen
    productTemplate <- forAllT productInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct productTemplate
        orderId <- unwrapInsert (Orders.insertOrder orderTemplate)
        itemTemplate <- pure $ UUT.Insert
          { iiOrderId = orderId,
            iiProductId = productId,
            iiVariantId = Nothing,
            iiProductName = Products.piName productTemplate,
            iiVariantLabel = "",
            iiQuantity = 2,
            iiUnitPriceCents = Products.piBasePriceCents productTemplate
          }
        itemId <- unwrapInsert (UUT.insertOrderItem itemTemplate)
        items <- TRX.statement () (UUT.getByOrderId orderId)
        TRX.condemn
        pure (itemId, items)

      assert $ do
        (itemId, items) <- assertRight result
        length items === 1
        let item = head items
        UUT.oiId item === itemId


prop_getByOrderIdEmpty :: TestDBConfig -> PropertyT IO ()
prop_getByOrderIdEmpty cfg = do
  arrange (bracketConn cfg) $ do
    orderTemplate <- forAllT orderInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (Orders.insertOrder orderTemplate)
        items <- TRX.statement () (UUT.getByOrderId orderId)
        TRX.condemn
        pure items

      assert $ do
        items <- assertRight result
        items === []


prop_getByOrderIdOrdered :: TestDBConfig -> PropertyT IO ()
prop_getByOrderIdOrdered cfg = do
  arrange (bracketConn cfg) $ do
    orderTemplate <- forAllT orderInsertGen
    productTemplate <- forAllT productInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct productTemplate
        orderId <- unwrapInsert (Orders.insertOrder orderTemplate)
        let mkItem name = UUT.Insert
              { iiOrderId = orderId,
                iiProductId = productId,
                iiVariantId = Nothing,
                iiProductName = name,
                iiVariantLabel = "",
                iiQuantity = 1,
                iiUnitPriceCents = Products.piBasePriceCents productTemplate
              }
        id1 <- unwrapInsert (UUT.insertOrderItem (mkItem "Item A"))
        id2 <- unwrapInsert (UUT.insertOrderItem (mkItem "Item B"))
        items <- TRX.statement () (UUT.getByOrderId orderId)
        TRX.condemn
        pure (id1, id2, items)

      assert $ do
        (id1, id2, items) <- assertRight result
        length items === 2
        UUT.oiId (head items) === id1
        UUT.oiId (items !! 1) === id2

--------------------------------------------------------------------------------
-- Fields

prop_insertFieldsMatch :: TestDBConfig -> PropertyT IO ()
prop_insertFieldsMatch cfg = do
  arrange (bracketConn cfg) $ do
    orderTemplate <- forAllT orderInsertGen
    productTemplate <- forAllT productInsertGen
    itemTemplate <- forAllT (orderItemInsertGen (Orders.Id 0) (Products.Id 0) Nothing)
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        productId <- insertTestProduct productTemplate
        orderId <- unwrapInsert (Orders.insertOrder orderTemplate)
        let fixedTemplate = itemTemplate
              { UUT.iiOrderId = orderId,
                UUT.iiProductId = productId
              }
        itemId <- unwrapInsert (UUT.insertOrderItem fixedTemplate)
        items <- TRX.statement () (UUT.getByOrderId orderId)
        TRX.condemn
        pure (itemId, fixedTemplate, items)

      assert $ do
        (itemId, fixedTemplate, items) <- assertRight result
        length items === 1
        let item = head items
        UUT.oiId item === itemId
        assertItemFieldsMatch fixedTemplate item
