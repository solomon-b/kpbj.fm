{-# LANGUAGE OverloadedStrings #-}

-- | WAI middleware that rate-limits specific public store API endpoints.
--
-- Uses @wai-rate-limit@ with a 'TVar'-backed in-memory store.
-- Limits are per-client-IP using the @CF-Connecting-IP@ header
-- (Cloudflare), falling back to @X-Forwarded-For@, then 'remoteHost'.
--
-- Rate-limited endpoints:
--
--   * @POST \/api\/store\/shipping-rates@ — 10 req/min (hits EasyPost)
--   * @POST \/api\/store\/checkout\/create-session@ — 5 req/min (creates orders)
--
-- All other requests pass through untouched.
module Middleware.RateLimit
  ( rateLimitMiddleware,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types.Header (HeaderName)
import Network.Socket (SockAddr)
import Network.Wai qualified as Wai
import Network.Wai.RateLimit (rateLimiting)
import Network.Wai.RateLimit.Backend (Backend (..))
import Network.Wai.RateLimit.Strategy (Strategy (..), fixedWindow)

--------------------------------------------------------------------------------

-- | Create rate limiting middleware for public store API endpoints.
--
-- Must be called once at startup (allocates in-memory state).
-- The returned 'Wai.Middleware' is safe to use across threads.
rateLimitMiddleware :: IO Wai.Middleware
rateLimitMiddleware = do
  shippingBackend <- newInMemoryBackend
  sessionBackend <- newInMemoryBackend
  let shippingLimiter = fixedWindow shippingBackend 60 10 getClientIP
      sessionLimiter = fixedWindow sessionBackend 60 5 getClientIP
      combinedStrategy =
        MkStrategy $ \req ->
          case (Wai.requestMethod req, Wai.pathInfo req) of
            ("POST", ["api", "store", "shipping-rates"]) ->
              strategyOnRequest shippingLimiter req
            ("POST", ["api", "store", "checkout", "create-session"]) ->
              strategyOnRequest sessionLimiter req
            _ -> pure True
  pure $ rateLimiting combinedStrategy

--------------------------------------------------------------------------------
-- Client IP extraction

-- | Extract the client IP from the request.
--
-- Checks headers in order: @CF-Connecting-IP@ (Cloudflare),
-- @X-Forwarded-For@ (first entry), then falls back to 'Wai.remoteHost'.
getClientIP :: Wai.Request -> IO ByteString
getClientIP req =
  pure $
    fromMaybe (sockAddrToBS $ Wai.remoteHost req) $
      lookup "CF-Connecting-IP" headers
        <> firstForwardedFor
  where
    headers :: [(HeaderName, ByteString)]
    headers = Wai.requestHeaders req

    firstForwardedFor :: Maybe ByteString
    firstForwardedFor = do
      xff <- lookup "X-Forwarded-For" headers
      -- Take the first (client) IP from the comma-separated list
      pure $ BS8.strip $ fst $ BS8.break (== ',') xff

    sockAddrToBS :: SockAddr -> ByteString
    sockAddrToBS = BS8.pack . show

--------------------------------------------------------------------------------
-- In-memory backend

type Store = TVar (Map ByteString (Integer, UTCTime))

-- | Create a new in-memory rate limiting backend.
--
-- Spawns a background thread that purges expired entries every 5 minutes
-- to prevent unbounded memory growth.
newInMemoryBackend :: IO (Backend ByteString)
newInMemoryBackend = do
  store <- newTVarIO Map.empty

  -- Periodic cleanup of expired entries
  void $ Async.async $ forever $ do
    threadDelay (5 * 60 * 1000000)
    now <- getCurrentTime
    atomically $ modifyTVar' store (Map.filter (\(_, expiry) -> now <= expiry))

  pure
    MkBackend
      { backendGetUsage = getUsage store,
        backendIncAndGetUsage = incAndGetUsage store,
        backendExpireIn = expireIn store
      }

-- | Get current usage for a key. Returns 0 if expired or absent.
getUsage :: Store -> ByteString -> IO Integer
getUsage store key = do
  now <- getCurrentTime
  atomically $ do
    m <- readTVar store
    case Map.lookup key m of
      Nothing -> pure 0
      Just (count, expiry)
        | now > expiry -> do
            modifyTVar' store (Map.delete key)
            pure 0
        | otherwise -> pure count

-- | Atomically increment usage and return the new count.
-- Expired entries are treated as absent (count starts from 0).
incAndGetUsage :: Store -> ByteString -> Integer -> IO Integer
incAndGetUsage store key delta = do
  now <- getCurrentTime
  atomically $ do
    m <- readTVar store
    let entry = Map.lookup key m
        current = case entry of
          Nothing -> 0
          Just (count, ex)
            | now > ex -> 0
            | otherwise -> count
        new = current + delta
        -- Preserve existing expiry if still valid; otherwise use a
        -- placeholder that backendExpireIn will overwrite immediately.
        newExpiry = case entry of
          Just (_, e) | now <= e -> e
          _ -> now
    writeTVar store (Map.insert key (new, newExpiry) m)
    pure new

-- | Set a key to expire in the given number of seconds from now.
expireIn :: Store -> ByteString -> Integer -> IO ()
expireIn store key seconds = do
  now <- getCurrentTime
  let expiry = addUTCTime (fromInteger seconds) now
  atomically $ modifyTVar' store (Map.adjust (\(count, _) -> (count, expiry)) key)

--------------------------------------------------------------------------------
