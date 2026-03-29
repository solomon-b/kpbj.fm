module API.Store.Products.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Store.Products.Slug.Get.Handler (ProductDetailData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestProduct)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Store.Products.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns product data for a valid slug" test_validSlugReturnsProduct
        it "returns NotFound for a nonexistent slug" test_notFoundForMissingSlug
        it "returns NotFound for an inactive product" test_notFoundForInactiveProduct

--------------------------------------------------------------------------------

-- | A minimal active product insert with a known slug.
mkProductInsert :: Products.Insert
mkProductInsert =
  Products.Insert
    { piName = "Test Product",
      piSlug = "test-product-detail",
      piDescription = "A test product for detail page testing.",
      piBasePriceCents = 1999,
      piWeightOz = 8,
      piCategory = Nothing,
      piInventoryCount = 10,
      piIsActive = True,
      piSortOrder = 0
    }

--------------------------------------------------------------------------------

-- | Fetching by a valid slug returns the product with all related data.
test_validSlugReturnsProduct :: TestDBConfig -> IO ()
test_validSlugReturnsProduct cfg = bracketAppM cfg $ do
  let slug = "valid-product-slug"
      ins = mkProductInsert {Products.piSlug = slug}

  dbResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $
        insertTestProduct ins

  case dbResult of
    Left err ->
      liftIO $ expectationFailure $ "Test setup failed: " <> show err
    Right productId -> do
      result <- runExceptT $ action slug
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd ->
          Products.pId (pddProduct vd) `shouldBe` productId

-- | Fetching by a slug that does not exist returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = bracketAppM cfg $ do
  result <- runExceptT $ action "nonexistent-slug-that-will-never-exist"
  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err ->
      expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ ->
      expectationFailure "Expected Left NotFound but got Right"

-- | An inactive product is treated as not found.
test_notFoundForInactiveProduct :: TestDBConfig -> IO ()
test_notFoundForInactiveProduct cfg = bracketAppM cfg $ do
  let slug = "inactive-product-slug"
      ins = mkProductInsert {Products.piSlug = slug, Products.piIsActive = False}

  dbResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $
        insertTestProduct ins

  case dbResult of
    Left err ->
      liftIO $ expectationFailure $ "Test setup failed: " <> show err
    Right _productId -> do
      result <- runExceptT $ action slug
      liftIO $ case result of
        Left (NotFound _) -> pure ()
        Left err ->
          expectationFailure $ "Expected NotFound but got: " <> show err
        Right _ ->
          expectationFailure "Expected Left NotFound but got Right"
