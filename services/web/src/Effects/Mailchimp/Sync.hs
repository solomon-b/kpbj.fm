{-# LANGUAGE QuasiQuotes #-}

-- | Outbound Mailchimp sync effect.
--
-- Mirrors the shape of "Effects.Email.Send.sendAsync" — fire-and-forget,
-- captured 'LoggerEnv' for safe logging from the worker thread, and a 30s
-- per-call timeout. When a 'MailchimpClient' is not configured the operation
-- is logged and skipped so dev environments without credentials still boot.
--
-- On a successful 'Upsert', the subscriber row is stamped with the returned
-- subscriber hash and status. On any failure (timeout, ClientError, or a
-- thrown exception), the row is flipped to @mailchimp_status='error'@ so the
-- daily reconcile job can retry it.
--
-- 'Archive' fires after the row is already deleted from Postgres, so success
-- and failure both log only — there is no row left to mark.
module Effects.Mailchimp.Sync
  ( -- * Operations
    SyncOp (..),

    -- * Dispatch
    syncAsync,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (getCurrentTime)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Hasql.Pool qualified
import Hasql.Session qualified
import Log qualified
import Mailchimp.Client (MailchimpClient (..), MailchimpError (..))
import Mailchimp.Types (Member (..), memberStatusTag)

--------------------------------------------------------------------------------

-- | A single Mailchimp sync operation.
--
-- Each constructor pairs the email address (so the worker thread doesn't have
-- to look the row up again) with the row's primary key (used to mark the row
-- as synced or errored after the call returns).
data SyncOp
  = -- | Push the subscriber to Mailchimp via @PUT /lists/{list_id}/members/{hash}@.
    Upsert EmailAddress NewsletterSubscribers.Id
  | -- | Archive the subscriber in Mailchimp via @DELETE /lists/{list_id}/members/{hash}@.
    --
    -- The row in Postgres has already been deleted before dispatch; this is
    -- purely the Mailchimp-side cleanup.
    Archive EmailAddress NewsletterSubscribers.Id
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Fire-and-forget Mailchimp sync.
--
-- When the 'MailchimpClient' is 'Nothing' (dev / unconfigured), logs and
-- skips — same convention as @Effects.Email.Send.sendAsync@'s
-- 'Maybe SmtpConfig' check. The Postgres row stays at
-- @mailchimp_status='pending'@ so the next reconcile job can pick it up.
--
-- Otherwise spawns an unbounded async, races a 30s timer against the API
-- call, then writes the success or failure marker to the row.
syncAsync ::
  ( MonadIO m,
    MonadReader r m,
    Has.Has (Maybe MailchimpClient) r,
    Has.Has Hasql.Pool.Pool r,
    Log.MonadLog m
  ) =>
  SyncOp ->
  m ()
syncAsync op =
  asks Has.getter >>= \case
    Nothing ->
      Log.logInfo
        [i|Mailchimp sync skipped (not configured): #{describeOp op}|]
        (display (opEmail op))
    Just client -> do
      pool <- asks Has.getter
      logEnv <- Log.getLoggerEnv
      Log.logInfo [i|Queuing Mailchimp #{describeOp op}|] (display (opEmail op))
      liftIO $ void $ Async.async $ runOp logEnv pool client op

--------------------------------------------------------------------------------
-- Internal helpers

-- | Run a single sync operation in the worker thread.
--
-- Races a 30s timeout against the actual API call. On timeout, exception, or
-- 'ClientError', flips the row to @error@ for 'Upsert' or just logs for
-- 'Archive' (where there is no row left to mark).
runOp ::
  Log.LoggerEnv ->
  Hasql.Pool.Pool ->
  MailchimpClient ->
  SyncOp ->
  IO ()
runOp logEnv pool MailchimpClient {..} op = case op of
  Upsert email subId -> do
    result <-
      Async.race
        (threadDelay thirtySeconds)
        (try @SomeException (mcUpsertMember email))
    case result of
      Left () -> do
        logIO logEnv Log.LogAttention [i|Timeout upserting Mailchimp member|] (display email)
        markErrorOrLog pool logEnv subId email "timeout"
      Right (Left e) -> do
        logIO logEnv Log.LogAttention [i|Exception upserting Mailchimp member: #{show e}|] (display email)
        markErrorOrLog pool logEnv subId email (Text.pack (show e))
      Right (Right (Left (MailchimpError clientErr))) -> do
        logIO logEnv Log.LogAttention [i|Mailchimp upsert failed: #{show clientErr}|] (display email)
        markErrorOrLog pool logEnv subId email (Text.pack (show clientErr))
      Right (Right (Right Member {..})) -> do
        let statusText = memberStatusTag mStatus
        markResult <-
          try @SomeException $
            Hasql.Pool.use pool $
              Hasql.Session.statement () (NewsletterSubscribers.markSynced subId mId statusText)
        case markResult of
          Left e ->
            logIO logEnv Log.LogAttention [i|Mailchimp upsert succeeded but row update threw: #{show e}|] (display email)
          Right (Left poolErr) ->
            logIO logEnv Log.LogAttention [i|Mailchimp upsert succeeded but row update failed: #{show poolErr}|] (display email)
          Right (Right ()) ->
            logIO logEnv Log.LogInfo [i|Mailchimp upsert synced|] (display email)
  Archive email _subId -> do
    result <-
      Async.race
        (threadDelay thirtySeconds)
        (try @SomeException (mcArchiveMember email))
    case result of
      Left () ->
        logIO logEnv Log.LogAttention [i|Timeout archiving Mailchimp member|] (display email)
      Right (Left e) ->
        logIO logEnv Log.LogAttention [i|Exception archiving Mailchimp member: #{show e}|] (display email)
      Right (Right (Left (MailchimpError clientErr))) ->
        logIO logEnv Log.LogAttention [i|Mailchimp archive failed: #{show clientErr}|] (display email)
      Right (Right (Right ())) ->
        logIO logEnv Log.LogInfo [i|Mailchimp archive succeeded|] (display email)

-- | Mark the row as @mailchimp_status='error'@; log on failure.
--
-- Called after an 'Upsert' API call fails. We never want a sync failure to
-- escape the worker thread, so any pool error here is logged and dropped.
markErrorOrLog ::
  Hasql.Pool.Pool ->
  Log.LoggerEnv ->
  NewsletterSubscribers.Id ->
  EmailAddress ->
  Text ->
  IO ()
markErrorOrLog pool logEnv subId email reason = do
  result <-
    try @SomeException $
      Hasql.Pool.use pool $
        Hasql.Session.statement () (NewsletterSubscribers.markError subId)
  case result of
    Left e ->
      logIO
        logEnv
        Log.LogAttention
        [i|markError raised after Mailchimp failure (#{reason}): #{show e}|]
        (display email)
    Right (Left poolErr) ->
      logIO
        logEnv
        Log.LogAttention
        [i|markError pool error after Mailchimp failure (#{reason}): #{show poolErr}|]
        (display email)
    Right (Right ()) ->
      logIO logEnv Log.LogInfo [i|Mailchimp row marked error (#{reason})|] (display email)

-- | One-line label for log lines.
describeOp :: SyncOp -> Text
describeOp = \case
  Upsert _ _ -> "upsert"
  Archive _ _ -> "archive"

-- | Pull the email out of a 'SyncOp' for logging.
opEmail :: SyncOp -> EmailAddress
opEmail = \case
  Upsert email _ -> email
  Archive email _ -> email

-- | 30 seconds expressed in microseconds for 'threadDelay'.
thirtySeconds :: Int
thirtySeconds = 30 * 1000000

-- | Log a message from plain IO using a captured 'LoggerEnv'.
--
-- Same shape as the helper in "Effects.Email.Send".
logIO :: (Aeson.ToJSON a) => Log.LoggerEnv -> Log.LogLevel -> Text -> a -> IO ()
logIO logEnv level msg payload = do
  now <- getCurrentTime
  Log.logMessageIO logEnv now level msg (Aeson.toJSON payload)
