module API.Store.List.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Store.List.Get.Handler (StoreListViewData (..), action)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Data.Text qualified as Text
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Products qualified as Products
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestProduct)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.Products (productInsertGen)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Store.List.Get.Handler" $ do
      describe "action" $ do
        runs 1 . it "returns empty products list on empty database" $
          hedgehog . prop_emptyDB

        runs 5 . it "returns active products" $
          hedgehog . prop_returnsActiveProducts

        runs 5 . it "excludes inactive products" $
          hedgehog . prop_excludesInactiveProducts

--------------------------------------------------------------------------------

-- | Empty database returns an empty products list.
prop_emptyDB :: TestDBConfig -> PropertyT IO ()
prop_emptyDB cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT action
      assert $ do
        vd <- assertRight result
        slvProducts vd === []

-- | Inserting N active products causes them to all appear in the result.
prop_returnsActiveProducts :: TestDBConfig -> PropertyT IO ()
prop_returnsActiveProducts cfg = do
  arrange (bracketAppM cfg) $ do
    inserts <- forAllT $ Gen.list (Range.linear 1 5) productInsertGen

    act $ do
      -- Count products already in the DB before our inserts.
      beforeResult <- runExceptT action
      let beforeCount = either (const 0) (length . slvProducts) beforeResult

      -- Insert products, overriding isActive to True and giving unique slugs.
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        mapM_
          ( \(i, ins) ->
              insertTestProduct
                ins
                  { Products.piIsActive = True,
                    Products.piSlug = Products.piSlug ins <> "-active-" <> Text.pack (show i)
                  }
          )
          (zip [(0 :: Int) ..] inserts)

      afterResult <- runExceptT action

      assert $ do
        H.assert (isRight afterResult)
        vd <- assertRight afterResult
        length (slvProducts vd) === beforeCount + length inserts

-- | Inactive products do not appear in the store listing.
prop_excludesInactiveProducts :: TestDBConfig -> PropertyT IO ()
prop_excludesInactiveProducts cfg = do
  arrange (bracketAppM cfg) $ do
    ins <- forAllT productInsertGen

    act $ do
      beforeResult <- runExceptT action
      let beforeCount = either (const 0) (length . slvProducts) beforeResult

      -- Insert a single inactive product.
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertTestProduct
          ins
            { Products.piIsActive = False,
              Products.piSlug = Products.piSlug ins <> "-inactive"
            }

      afterResult <- runExceptT action

      assert $ do
        vd <- assertRight afterResult
        length (slvProducts vd) === beforeCount
