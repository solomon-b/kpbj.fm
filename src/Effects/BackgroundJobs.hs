{-# LANGUAGE ScopedTypeVariables #-}

-- | Background job runner for periodic tasks.
--
-- Uses the async library to run cleanup jobs concurrently with the main server.
-- Jobs run on a configurable interval and handle their own errors gracefully.
--
-- This module provides a standalone cleanup loop that connects to the database
-- independently of the main server, allowing it to be started alongside the
-- server using 'Control.Concurrent.Async.withAsync'.
module Effects.BackgroundJobs
  ( -- * Job Runner
    runCleanupLoop,

    -- * Configuration
    defaultCleanupIntervalSeconds,
    tokenRetentionDays,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Int (Int64)
import Effects.Database.Tables.EmailVerificationTokens qualified as VerificationTokens
import Effects.Database.Tables.PasswordResetTokens qualified as PasswordResetTokens
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session

--------------------------------------------------------------------------------

-- | Default interval between cleanup runs in seconds (1 hour).
--
-- Can be overridden via the @APP_CLEANUP_INTERVAL_SECONDS@ environment variable.
defaultCleanupIntervalSeconds :: Int
defaultCleanupIntervalSeconds = 3600

-- | Number of days to retain tokens before purging.
--
-- Tokens older than this (regardless of status) are deleted for data hygiene.
tokenRetentionDays :: Int64
tokenRetentionDays = 90

--------------------------------------------------------------------------------

-- | Run the cleanup loop forever using the provided database pool.
--
-- This function runs cleanup tasks periodically. It should be started
-- with 'Control.Concurrent.Async.withAsync' before the main server:
--
-- @
-- interval <- getCleanupInterval
-- Async.withAsync (runCleanupLoop interval pool) $ \_ -> do
--   runMainServer
-- @
runCleanupLoop ::
  -- | Cleanup interval in seconds
  Int ->
  -- | Database pool
  Pool.Pool ->
  IO ()
runCleanupLoop intervalSeconds pool = do
  putStrLn $ "[BackgroundJobs] Starting cleanup loop (interval: " <> show intervalSeconds <> "s)"

  -- Run cleanup immediately on startup
  runCleanupTasks pool

  -- Then run periodically
  forever $ do
    threadDelay (intervalSeconds * 1000000)
    runCleanupTasks pool

-- | Run all cleanup tasks, catching and logging any exceptions.
runCleanupTasks :: Pool.Pool -> IO ()
runCleanupTasks pool = do
  -- Clean up expired pending tokens (not yet verified/used but past expiration)
  cleanupWithLogging pool "expired email verification tokens" $
    Session.statement () VerificationTokens.deleteExpired

  cleanupWithLogging pool "expired password reset tokens" $
    Session.statement () PasswordResetTokens.deleteExpired

  -- Purge old tokens regardless of status (audit trail retention limit)
  cleanupWithLogging pool ("email verification tokens older than " <> show tokenRetentionDays <> " days") $
    Session.statement () (VerificationTokens.deleteOlderThanDays tokenRetentionDays)

  cleanupWithLogging pool ("password reset tokens older than " <> show tokenRetentionDays <> " days") $
    Session.statement () (PasswordResetTokens.deleteOlderThanDays tokenRetentionDays)

-- | Run a cleanup session with error handling and logging.
cleanupWithLogging :: Pool.Pool -> String -> Session.Session () -> IO ()
cleanupWithLogging pool name session = do
  result <- try $ Pool.use pool session
  case result of
    Left (e :: SomeException) ->
      putStrLn $ "[BackgroundJobs] " <> name <> " cleanup failed: " <> show e
    Right (Left poolErr) ->
      putStrLn $ "[BackgroundJobs] " <> name <> " cleanup pool error: " <> show poolErr
    Right (Right ()) ->
      putStrLn $ "[BackgroundJobs] Cleaned up expired " <> name
