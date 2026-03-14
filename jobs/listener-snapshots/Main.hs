module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  mDbUrl <- lookupEnv "DATABASE_URL"
  mIcecastUrl <- lookupEnv "ICECAST_METADATA_URL"

  case (mDbUrl, mIcecastUrl) of
    (Nothing, _) -> do
      hPutStrLn stderr "ERROR: DATABASE_URL environment variable is required"
      exitFailure
    (_, Nothing) -> do
      hPutStrLn stderr "ERROR: ICECAST_METADATA_URL environment variable is required"
      exitFailure
    (Just dbUrl, Just icecastUrl) -> do
      connResult <- Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
      case connResult of
        Left err -> do
          hPutStrLn stderr $ "ERROR: Failed to connect to database: " <> show err
          exitFailure
        Right conn ->
          bracket (pure conn) Connection.release $ \c ->
            Log.withStdOutLogger $ \logger ->
              Log.runLogT "listener-snapshots" logger Log.LogInfo $
                pollAndRecord c icecastUrl

--------------------------------------------------------------------------------

-- | Fetch Icecast stats and record the listener count.
pollAndRecord :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> String -> m ()
pollAndRecord conn icecastUrl = do
  mListenerCount <- liftIO $ fetchListenerCount icecastUrl
  case mListenerCount of
    Nothing ->
      Log.logAttention_ "Icecast unreachable or unparseable, skipping snapshot"
    Just count -> do
      result <- liftIO $ try @SomeException $ Session.run (insertSnapshot count) conn
      case result of
        Left err ->
          Log.logAttention "Insert failed" (show err)
        Right (Left sessionErr) ->
          Log.logAttention "Insert error" (show sessionErr)
        Right (Right ()) ->
          Log.logInfo "Recorded snapshot" (Aeson.object ["listener_count" Aeson..= count])

--------------------------------------------------------------------------------

-- | Fetch the current listener count from Icecast's JSON status endpoint.
--
-- Handles both single-source (JSON object) and multi-source (JSON array)
-- responses. Returns Nothing if Icecast is unreachable or the response
-- cannot be parsed.
fetchListenerCount :: String -> IO (Maybe Int32)
fetchListenerCount url = do
  result <- try @HTTP.HttpException $ do
    request <- HTTP.parseRequest url
    let request' = HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 5_000_000) request
    response <- HTTP.httpLBS request'
    pure (HTTP.getResponseBody response)

  case result of
    Left _err -> pure Nothing
    Right body -> pure (parseListenerCount body)


-- | Parse listener count from Icecast JSON.
--
-- Icecast returns @icestats.source@ as either:
--   - A JSON object (single mountpoint)
--   - A JSON array (multiple mountpoints) — we take the first source
parseListenerCount :: LBS.ByteString -> Maybe Int32
parseListenerCount body = do
  json <- Aeson.decode body
  case json of
    Object root -> do
      Object icestats <- KeyMap.lookup "icestats" root
      source <- case KeyMap.lookup "source" icestats of
        Just (Object src) -> Just src
        Just (Array arr) -> case toList arr of
          (Object src : _) -> Just src
          _ -> Nothing
        _ -> Nothing
      Number n <- KeyMap.lookup (fromText "listeners") source
      pure (round n)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Database

-- | Insert a listener snapshot row.
insertSnapshot :: Int32 -> Session.Session ()
insertSnapshot count =
  Session.statement count $
    Statement.Statement
      "INSERT INTO listener_snapshots (listener_count) VALUES ($1 :: int4)"
      (Encoders.param (Encoders.nonNullable Encoders.int4))
      Decoders.noResult
      True
