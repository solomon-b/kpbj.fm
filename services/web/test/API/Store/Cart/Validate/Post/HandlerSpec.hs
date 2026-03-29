module API.Store.Cart.Validate.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Store.Cart.Validate.Post.Handler (validateCartItems)
import API.Store.Cart.Validate.Post.Templates (ValidatedCartItem (..))
import API.Store.Types (CartItem (..))
import Control.Monad.IO.Class (liftIO)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestProduct)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Store.Cart.Validate.Post.Handler" $ do
      describe "validateCartItems" $ do
        it "validates a valid cart item" test_validCartItem
        it "warns on missing product" test_missingProduct
        it "warns on inactive product" test_inactiveProduct
        it "clamps quantity to available inventory" test_quantityClamping

--------------------------------------------------------------------------------

-- | A minimal active product insert for cart tests.
mkCartProductInsert :: Products.Insert
mkCartProductInsert =
  Products.Insert
    { piName = "Cart Test Product",
      piSlug = "cart-test-product",
      piDescription = "A product for cart validation testing.",
      piBasePriceCents = 2500,
      piWeightOz = 4,
      piCategory = Nothing,
      piInventoryCount = 10,
      piIsActive = True,
      piSortOrder = 0
    }

--------------------------------------------------------------------------------

-- | A valid cart item for an existing, active product returns a validated item
-- with no warnings.
test_validCartItem :: TestDBConfig -> IO ()
test_validCartItem cfg = bracketAppM cfg $ do
  let ins = mkCartProductInsert {Products.piSlug = "cart-valid-item"}

  dbResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $
        insertTestProduct ins

  case dbResult of
    Left err ->
      liftIO $ expectSetupFailure err
    Right productId -> do
      let cart = [CartItem {productId = productId, variantId = Nothing, quantity = 2}]
      (items, warnings) <- validateCartItems cart
      liftIO $ case items of
        [item] -> do
          warnings `shouldSatisfy` null
          vciProductId item `shouldBe` productId
          vciQuantity item `shouldBe` 2
        _ -> fail $ "Expected exactly 1 validated item but got " <> show (length items)

-- | A cart item referencing a nonexistent product ID produces a warning
-- and no validated items.
test_missingProduct :: TestDBConfig -> IO ()
test_missingProduct cfg = bracketAppM cfg $ do
  let cart = [CartItem {productId = Products.Id 999999999, variantId = Nothing, quantity = 1}]
  (items, warnings) <- validateCartItems cart
  liftIO $ do
    items `shouldSatisfy` null
    length warnings `shouldBe` 1

-- | A cart item referencing an inactive product produces a warning
-- and no validated items.
test_inactiveProduct :: TestDBConfig -> IO ()
test_inactiveProduct cfg = bracketAppM cfg $ do
  let ins = mkCartProductInsert {Products.piSlug = "cart-inactive-product", Products.piIsActive = False}

  dbResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $
        insertTestProduct ins

  case dbResult of
    Left err ->
      liftIO $ expectSetupFailure err
    Right productId -> do
      let cart = [CartItem {productId = productId, variantId = Nothing, quantity = 1}]
      (items, warnings) <- validateCartItems cart
      liftIO $ do
        items `shouldSatisfy` null
        length warnings `shouldBe` 1

-- | When requested quantity exceeds available inventory, the quantity
-- is clamped down and a warning is produced.
test_quantityClamping :: TestDBConfig -> IO ()
test_quantityClamping cfg = bracketAppM cfg $ do
  let ins = mkCartProductInsert {Products.piSlug = "cart-clamp-product", Products.piInventoryCount = 3}

  dbResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $
        insertTestProduct ins

  case dbResult of
    Left err ->
      liftIO $ expectSetupFailure err
    Right productId -> do
      let cart = [CartItem {productId = productId, variantId = Nothing, quantity = 10}]
      (items, warnings) <- validateCartItems cart
      liftIO $ case items of
        [item] -> do
          vciQuantity item `shouldBe` 3
          length warnings `shouldBe` 1
        _ -> fail $ "Expected exactly 1 validated item but got " <> show (length items)

--------------------------------------------------------------------------------

-- | Fail the test with a descriptive message when setup goes wrong.
expectSetupFailure :: (Show e) => e -> IO a
expectSetupFailure err = fail $ "Test setup failed: " <> show err
