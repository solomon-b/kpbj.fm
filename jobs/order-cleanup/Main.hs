module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text qualified as Text
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Session qualified as Session
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

newtype Options = Options {optDryRun :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "Print what would be cancelled without cancelling")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Cancel stale pending orders and restore inventory"))
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mDbUrl of
    Nothing -> do
      hPutStrLn stderr "ERROR: DATABASE_URL environment variable is required"
      exitFailure
    Just dbUrl -> do
      connResult <- Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
      case connResult of
        Left err -> do
          hPutStrLn stderr $ "ERROR: Failed to connect to database: " <> show err
          exitFailure
        Right conn ->
          bracket (pure conn) Connection.release $ \c ->
            Log.withStdOutLogger $ \logger ->
              Log.runLogT "order-cleanup" logger Log.LogInfo $
                if optDryRun opts
                  then runDryRun c
                  else runCleanup c

--------------------------------------------------------------------------------

runDryRun :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> m ()
runDryRun conn = do
  Log.logInfo "DRY RUN — no orders will be cancelled" ()
  result <- liftIO $ try $ Session.run (Session.statement () Orders.cancelStalePendingOrders) conn
  case result of
    Left (e :: SomeException) ->
      Log.logAttention "Query failed" $ Aeson.object ["error" .= show e]
    Right (Left sessionErr) ->
      Log.logAttention "Session error" $ Aeson.object ["error" .= show sessionErr]
    Right (Right []) ->
      Log.logInfo "No stale pending orders found" ()
    Right (Right orderIds) ->
      Log.logInfo "Would cancel stale orders" $ Aeson.object ["count" .= length orderIds, "order_ids" .= map Orders.unId orderIds]

runCleanup :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> m ()
runCleanup conn = do
  result <- liftIO $ try $ Session.run (Session.statement () Orders.cancelStalePendingOrders) conn
  case result of
    Left (e :: SomeException) -> do
      Log.logAttention "Cancel query failed" $ Aeson.object ["error" .= show e]
      liftIO exitFailure
    Right (Left sessionErr) -> do
      Log.logAttention "Cancel session error" $ Aeson.object ["error" .= show sessionErr]
      liftIO exitFailure
    Right (Right []) ->
      Log.logInfo "No stale pending orders to clean up" ()
    Right (Right orderIds) -> do
      Log.logInfo "Cancelled stale orders" $ Aeson.object ["count" .= length orderIds]
      forM_ orderIds $ \orderId -> restoreOrderInventory conn orderId

-- | Fetch the items for a cancelled order and restore inventory for each.
restoreOrderInventory :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> Orders.Id -> m ()
restoreOrderInventory conn orderId = do
  result <- liftIO $ try $ Session.run (Session.statement () (OrderItems.getByOrderId orderId)) conn
  case result of
    Left (e :: SomeException) ->
      Log.logAttention "Failed to fetch order items" $ Aeson.object ["order_id" .= Orders.unId orderId, "error" .= show e]
    Right (Left sessionErr) ->
      Log.logAttention "Session error fetching items" $ Aeson.object ["order_id" .= Orders.unId orderId, "error" .= show sessionErr]
    Right (Right items) ->
      forM_ items $ \item -> do
        let qty = OrderItems.oiQuantity item
            productId = OrderItems.oiProductId item
        case OrderItems.oiVariantId item of
          Just variantId -> restoreVariantInventory conn orderId variantId qty
          Nothing -> restoreProductInventory conn orderId productId qty

restoreProductInventory :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> Orders.Id -> Products.Id -> Int64 -> m ()
restoreProductInventory conn orderId productId qty = do
  result <- liftIO $ try $ Session.run (Session.statement () (Products.restoreInventory productId qty)) conn
  case result of
    Left (e :: SomeException) ->
      Log.logAttention "Failed to restore product inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "product_id" .= Products.unId productId, "error" .= show e]
    Right (Left sessionErr) ->
      Log.logAttention "Session error restoring product inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "product_id" .= Products.unId productId, "error" .= show sessionErr]
    Right (Right ()) ->
      Log.logInfo "Restored product inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "product_id" .= Products.unId productId, "quantity" .= qty]

restoreVariantInventory :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> Orders.Id -> ProductVariants.Id -> Int64 -> m ()
restoreVariantInventory conn orderId variantId qty = do
  result <- liftIO $ try $ Session.run (Session.statement () (ProductVariants.restoreInventory variantId qty)) conn
  case result of
    Left (e :: SomeException) ->
      Log.logAttention "Failed to restore variant inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "variant_id" .= ProductVariants.unId variantId, "error" .= show e]
    Right (Left sessionErr) ->
      Log.logAttention "Session error restoring variant inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "variant_id" .= ProductVariants.unId variantId, "error" .= show sessionErr]
    Right (Right ()) ->
      Log.logInfo "Restored variant inventory" $
        Aeson.object ["order_id" .= Orders.unId orderId, "variant_id" .= ProductVariants.unId variantId, "quantity" .= qty]
