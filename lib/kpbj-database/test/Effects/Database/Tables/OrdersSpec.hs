module Effects.Database.Tables.OrdersSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Orders qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.Orders (genOrderNumber, genOrderStatus, orderInsertGen)
import Test.Gen.Text (genText)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Orders" $ do
      describe "Insert and Select" $ do
        runs 10 . it "insert-getById: inserted fields preserved on select" $
          hedgehog . prop_insertGetById
        runs 10 . it "insert-getByOrderNumber: can retrieve by order number" $
          hedgehog . prop_insertGetByOrderNumber

      describe "Stripe Lookups" $ do
        runs 10 . it "getByStripeCheckoutSessionId: returns order when session id set" $
          hedgehog . prop_getByStripeCheckoutSessionId
        runs 10 . it "getByStripePaymentIntentId: returns order when payment intent set" $
          hedgehog . prop_getByStripePaymentIntentId

      describe "Updates" $ do
        runs 10 . it "updateStatus: changes status and returns updated order" $
          hedgehog . prop_updateStatus
        runs 10 . it "updateStripeCheckoutSessionId: sets the session id" $
          hedgehog . prop_updateStripeCheckoutSessionId
        runs 10 . it "updateStripePaymentIntentId: sets the payment intent id" $
          hedgehog . prop_updateStripePaymentIntentId
        runs 10 . it "updateTracking: sets tracking info" $
          hedgehog . prop_updateTracking
        runs 10 . it "updateNotes: sets notes" $
          hedgehog . prop_updateNotes

      describe "List" $ do
        runs 10 . it "listOrders Nothing: returns all orders" $
          hedgehog . prop_listOrdersAll
        runs 10 . it "listOrders (Just status): filters by status" $
          hedgehog . prop_listOrdersFiltered

--------------------------------------------------------------------------------
-- Helpers

assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.oiOrderNumber insert === UUT.oOrderNumber model
  UUT.oiEmail insert === UUT.oEmail model
  UUT.oiShippingFirstName insert === UUT.oShippingFirstName model
  UUT.oiShippingLastName insert === UUT.oShippingLastName model
  UUT.oiShippingAddressLine1 insert === UUT.oShippingAddressLine1 model
  UUT.oiShippingAddressLine2 insert === UUT.oShippingAddressLine2 model
  UUT.oiShippingCity insert === UUT.oShippingCity model
  UUT.oiShippingState insert === UUT.oShippingState model
  UUT.oiShippingZip insert === UUT.oShippingZip model
  UUT.oiShippingCountry insert === UUT.oShippingCountry model
  UUT.oiShippingMethod insert === UUT.oShippingMethod model
  UUT.oiSubtotalCents insert === UUT.oSubtotalCents model
  UUT.oiShippingCents insert === UUT.oShippingCents model
  UUT.oiTaxCents insert === UUT.oTaxCents model
  UUT.oiTotalCents insert === UUT.oTotalCents model
  UUT.oiPaymentMethod insert === UUT.oPaymentMethod model
  UUT.oiStripeCheckoutSessionId insert === UUT.oStripeCheckoutSessionId model
  UUT.oStatus model === UUT.Pending

--------------------------------------------------------------------------------
-- Insert and Select

prop_insertGetById :: TestDBConfig -> PropertyT IO ()
prop_insertGetById cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        selected <- TRX.statement () (UUT.getById orderId)
        TRX.condemn
        pure (orderId, selected)

      assert $ do
        (orderId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch template selected
        UUT.oId selected === orderId


prop_insertGetByOrderNumber :: TestDBConfig -> PropertyT IO ()
prop_insertGetByOrderNumber cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        byNumber <- TRX.statement () (UUT.getByOrderNumber $ UUT.oiOrderNumber template)
        TRX.condemn
        pure (orderId, byNumber)

      assert $ do
        (orderId, mByNumber) <- assertRight result
        byNumber <- assertJust mByNumber
        UUT.oId byNumber === orderId
        UUT.oOrderNumber byNumber === UUT.oiOrderNumber template

--------------------------------------------------------------------------------
-- Stripe Lookups

prop_getByStripeCheckoutSessionId :: TestDBConfig -> PropertyT IO ()
prop_getByStripeCheckoutSessionId cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    sessionId <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let withSession = template {UUT.oiStripeCheckoutSessionId = Just sessionId}
        orderId <- unwrapInsert (UUT.insertOrder withSession)
        found <- TRX.statement () (UUT.getByStripeCheckoutSessionId sessionId)
        notFound <- TRX.statement () (UUT.getByStripeCheckoutSessionId "nonexistent")
        TRX.condemn
        pure (orderId, found, notFound)

      assert $ do
        (orderId, mFound, mNotFound) <- assertRight result
        found <- assertJust mFound
        UUT.oId found === orderId
        assertNothing mNotFound


prop_getByStripePaymentIntentId :: TestDBConfig -> PropertyT IO ()
prop_getByStripePaymentIntentId cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    paymentIntentId <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        TRX.statement () (UUT.updateStripePaymentIntentId orderId paymentIntentId)
        found <- TRX.statement () (UUT.getByStripePaymentIntentId paymentIntentId)
        notFound <- TRX.statement () (UUT.getByStripePaymentIntentId "nonexistent")
        TRX.condemn
        pure (orderId, found, notFound)

      assert $ do
        (orderId, mFound, mNotFound) <- assertRight result
        found <- assertJust mFound
        UUT.oId found === orderId
        assertNothing mNotFound

--------------------------------------------------------------------------------
-- Updates

prop_updateStatus :: TestDBConfig -> PropertyT IO ()
prop_updateStatus cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    newStatus <- forAllT genOrderStatus

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        updated <- TRX.statement () (UUT.updateStatus orderId newStatus)
        TRX.condemn
        pure updated

      assert $ do
        mUpdated <- assertRight result
        updated <- assertJust mUpdated
        UUT.oStatus updated === newStatus


prop_updateStripeCheckoutSessionId :: TestDBConfig -> PropertyT IO ()
prop_updateStripeCheckoutSessionId cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    sessionId <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        TRX.statement () (UUT.updateStripeCheckoutSessionId orderId sessionId)
        selected <- TRX.statement () (UUT.getById orderId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.oStripeCheckoutSessionId selected === Just sessionId


prop_updateStripePaymentIntentId :: TestDBConfig -> PropertyT IO ()
prop_updateStripePaymentIntentId cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    paymentIntentId <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        TRX.statement () (UUT.updateStripePaymentIntentId orderId paymentIntentId)
        selected <- TRX.statement () (UUT.getById orderId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.oStripePaymentIntentId selected === Just paymentIntentId


prop_updateTracking :: TestDBConfig -> PropertyT IO ()
prop_updateTracking cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    trackingNumber <- forAllT genText
    shipmentId <- forAllT genText
    labelUrl <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        TRX.statement () (UUT.updateTracking orderId trackingNumber (Just shipmentId) (Just labelUrl))
        selected <- TRX.statement () (UUT.getById orderId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.oTrackingNumber selected === Just trackingNumber
        UUT.oEasypostShipmentId selected === Just shipmentId
        UUT.oLabelUrl selected === Just labelUrl


prop_updateNotes :: TestDBConfig -> PropertyT IO ()
prop_updateNotes cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT orderInsertGen
    notes <- forAllT genText

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        orderId <- unwrapInsert (UUT.insertOrder template)
        TRX.statement () (UUT.updateNotes orderId notes)
        selected <- TRX.statement () (UUT.getById orderId)
        TRX.condemn
        pure selected

      assert $ do
        mSelected <- assertRight result
        selected <- assertJust mSelected
        UUT.oNotes selected === notes

--------------------------------------------------------------------------------
-- List

prop_listOrdersAll :: TestDBConfig -> PropertyT IO ()
prop_listOrdersAll cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT orderInsertGen
    template2 <- forAllT orderInsertGen

    baseOrderNumber <- forAllT genOrderNumber

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let t1 = template1 {UUT.oiOrderNumber = baseOrderNumber <> "-1"}
        let t2 = template2 {UUT.oiOrderNumber = baseOrderNumber <> "-2"}
        id1 <- unwrapInsert (UUT.insertOrder t1)
        id2 <- unwrapInsert (UUT.insertOrder t2)
        allOrders <- TRX.statement () (UUT.listOrders Nothing)
        TRX.condemn
        pure (id1, id2, allOrders)

      assert $ do
        (id1, id2, allOrders) <- assertRight result
        length allOrders === 2
        let returnedIds = map UUT.oId allOrders
        elem id1 returnedIds === True
        elem id2 returnedIds === True


prop_listOrdersFiltered :: TestDBConfig -> PropertyT IO ()
prop_listOrdersFiltered cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT orderInsertGen
    template2 <- forAllT orderInsertGen

    baseOrderNumber <- forAllT genOrderNumber

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let t1 = template1 {UUT.oiOrderNumber = baseOrderNumber <> "-1"}
        let t2 = template2 {UUT.oiOrderNumber = baseOrderNumber <> "-2"}
        id1 <- unwrapInsert (UUT.insertOrder t1)
        id2 <- unwrapInsert (UUT.insertOrder t2)
        -- Move order 2 to Paid status
        _ <- TRX.statement () (UUT.updateStatus id2 UUT.Paid)
        pendingOrders <- TRX.statement () (UUT.listOrders (Just UUT.Pending))
        paidOrders <- TRX.statement () (UUT.listOrders (Just UUT.Paid))
        TRX.condemn
        pure (id1, id2, pendingOrders, paidOrders)

      assert $ do
        (id1, id2, pendingOrders, paidOrders) <- assertRight result
        length pendingOrders === 1
        UUT.oId (head pendingOrders) === id1
        length paidOrders === 1
        UUT.oId (head paidOrders) === id2
