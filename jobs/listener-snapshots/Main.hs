{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Domain.Icecast.Status (parseListenerCount)
import Effects.Database.Execute (execQuery)
import Hasql.Interpolate (interp, sql)
import Hasql.Statement qualified as Hasql
import Job (JobM, requireEnv, runJob)
import Log qualified
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP

--------------------------------------------------------------------------------

main :: IO ()
main = do
  icecastUrl <- requireEnv "ICECAST_METADATA_URL"
  runJob "listener-snapshots" (pollAndRecord icecastUrl)

--------------------------------------------------------------------------------

-- | Fetch Icecast stats and record the listener count.
pollAndRecord :: String -> JobM ()
pollAndRecord icecastUrl = do
  mListenerCount <- liftIO $ fetchListenerCount icecastUrl
  case mListenerCount of
    Nothing ->
      Log.logAttention_ "Icecast unreachable or unparseable, skipping snapshot"
    Just count ->
      execQuery (insertSnapshot count) >>= \case
        Left err ->
          Log.logAttention "Insert error" (show err)
        Right () ->
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

--------------------------------------------------------------------------------
-- Database

-- | Insert a listener snapshot row.
insertSnapshot :: Int32 -> Hasql.Statement () ()
insertSnapshot count =
  interp
    True
    [sql|
    INSERT INTO listener_snapshots (listener_count) VALUES (#{count} :: int4)
  |]
