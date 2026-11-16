module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Job (JobM, runJob)
import Log qualified
import Options.Applicative
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

newtype Options = Options {optDryRun :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "Print what would be cancelled without cancelling")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Cancel stale pending orders and restore inventory"))
  runJob "order-cleanup" $
    if optDryRun opts
      then runDryRun
      else runCleanup

--------------------------------------------------------------------------------

runDryRun :: JobM ()
runDryRun = do
  Log.logInfo "DRY RUN — no orders will be cancelled" ()
  execQuery Orders.getStalePendingOrderIds >>= \case
    Left err ->
      Log.logAttention "Session error" $ Aeson.object ["error" .= show err]
    Right [] ->
      Log.logInfo "No stale pending orders found" ()
    Right orderIds ->
      Log.logInfo "Would cancel stale orders" $ Aeson.object ["count" .= length orderIds, "order_ids" .= map Orders.unId orderIds]

runCleanup :: JobM ()
runCleanup =
  execQuery Orders.cancelStalePendingOrders >>= \case
    Left err -> do
      Log.logAttention "Cancel session error" $ Aeson.object ["error" .= show err]
      liftIO exitFailure
    Right [] ->
      Log.logInfo "No stale pending orders to clean up" ()
    Right orderIds -> do
      Log.logInfo "Cancelled stale orders" $ Aeson.object ["count" .= length orderIds]
      forM_ orderIds restoreOrderInventory

-- | Fetch the items for a cancelled order and restore inventory for each.
restoreOrderInventory :: Orders.Id -> JobM ()
restoreOrderInventory orderId =
  execQuery (OrderItems.getByOrderId orderId) >>= \case
    Left err ->
      Log.logAttention "Session error fetching items" $ Aeson.object ["order_id" .= Orders.unId orderId, "error" .= show err]
    Right items ->
      forM_ items $ \item -> do
        let qty = OrderItems.oiQuantity item
            productId = OrderItems.oiProductId item
        case OrderItems.oiVariantId item of
          Just variantId -> restoreVariantInventory orderId variantId qty
          Nothing -> restoreProductInventory orderId productId qty

restoreProductInventory :: Orders.Id -> Products.Id -> Int64 -> JobM ()
restoreProductInventory orderId productId qty =
  execQuery (Products.restoreInventory productId qty) >>= \case
    Left err ->
      Log.logAttention "Session error restoring product inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "product_id" .= Products.unId productId, "error" .= show err]
    Right () ->
      Log.logInfo "Restored product inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "product_id" .= Products.unId productId, "quantity" .= qty]

restoreVariantInventory :: Orders.Id -> ProductVariants.Id -> Int64 -> JobM ()
restoreVariantInventory orderId variantId qty =
  execQuery (ProductVariants.restoreInventory variantId qty) >>= \case
    Left err ->
      Log.logAttention "Session error restoring variant inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "variant_id" .= ProductVariants.unId variantId, "error" .= show err]
    Right () ->
      Log.logInfo "Restored variant inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "variant_id" .= ProductVariants.unId variantId, "quantity" .= qty]
