-- | Bidirectional reconcile between Mailchimp and the local
-- @newsletter_subscribers@ table.
--
-- Two modes:
--
--   * @--bootstrap@ — one-shot import of all currently-subscribed Mailchimp
--     members into Postgres. Run once after the first deploy.
--   * default (steady-state, daily timer) — pulls every Mailchimp member,
--     compares against the Postgres rows, and:
--       - re-pushes any PG row missing from Mailchimp or whose
--         @mailchimp_status@ is currently @\'error\'@ or @NULL@,
--       - deletes any PG row whose Mailchimp record now reports
--         @unsubscribed@ or @cleaned@,
--       - logs a warning for every Mailchimp member that does not appear
--         in PG (manual review — we never auto-import in steady state).
--
-- Postgres is the post-bootstrap source of truth. The reconcile job exists
-- because the per-row outbound dispatch is fire-and-forget; this job is
-- the safety net for any sync that drifted.
module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Session qualified as Session
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Mailchimp.Client (MailchimpClient (..), MailchimpError (..), mkClient)
import Mailchimp.Config (MailchimpApiKey (..), MailchimpAudienceId (..))
import Mailchimp.Types
  ( ListMembersResponse (..),
    Member (..),
    MemberStatus (..),
    memberStatusTag,
  )
import Network.HTTP.Client.TLS qualified as TLS
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

-- | Top-level command derived from argv.
data Command
  = -- | One-time backfill: pull MC into PG.
    Bootstrap
  | -- | Default daily mode: diff PG ↔ MC and reconcile divergence.
    Reconcile

commandParser :: Parser Command
commandParser =
  flag
    Reconcile
    Bootstrap
    ( long "bootstrap"
        <> help "One-shot import of all subscribed Mailchimp members into Postgres."
    )

main :: IO ()
main = do
  cmd <-
    execParser
      ( info
          (commandParser <**> helper)
          ( fullDesc
              <> progDesc "Reconcile Mailchimp audience with newsletter_subscribers."
          )
      )
  mDbUrl <- lookupEnv "DATABASE_URL"
  mApiKey <- lookupEnv "MAILCHIMP_API_KEY"
  mAudienceId <- lookupEnv "MAILCHIMP_AUDIENCE_ID"
  case (mDbUrl, mApiKey, mAudienceId) of
    (Nothing, _, _) -> die "DATABASE_URL environment variable is required"
    (_, Nothing, _) -> die "MAILCHIMP_API_KEY environment variable is required"
    (_, _, Nothing) -> die "MAILCHIMP_AUDIENCE_ID environment variable is required"
    (Just dbUrl, Just apiKey, Just audienceId) -> do
      mgr <- TLS.newTlsManager
      let mcKey = MailchimpApiKey (BS8.pack apiKey)
          mcAudience = MailchimpAudienceId (Text.pack audienceId)
      client <- case mkClient mgr mcKey mcAudience of
        Left err -> die [i|Failed to construct Mailchimp client: #{Text.unpack err}|]
        Right c -> pure c
      connResult <-
        Connection.acquire
          [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
      case connResult of
        Left err -> die [i|Failed to connect to database: #{show err}|]
        Right conn ->
          bracket (pure conn) Connection.release $ \c ->
            Log.withStdOutLogger $ \logger ->
              Log.runLogT "mailchimp-reconcile" logger Log.LogInfo $
                runCommand c client cmd

-- | Print an error to stderr and exit with a non-zero status.
die :: String -> IO a
die msg = do
  hPutStrLn stderr [i|ERROR: #{msg}|]
  exitFailure

--------------------------------------------------------------------------------
-- Command dispatch

-- | Dispatch on the parsed 'Command'.
runCommand ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  MailchimpClient ->
  Command ->
  m ()
runCommand conn client = \case
  Bootstrap -> runBootstrap conn client
  Reconcile -> runReconcile conn client

--------------------------------------------------------------------------------
-- Bootstrap mode

-- | Pull every @subscribed@ Mailchimp member into Postgres.
--
-- Skips conflicts on email so this can be re-run safely.
runBootstrap ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  MailchimpClient ->
  m ()
runBootstrap conn client = do
  Log.logInfo_ "Bootstrap: fetching Mailchimp members"
  members <- fetchAllMembers client
  let isSubscribed Member {mStatus} = mStatus == Subscribed
      !subscribed = filter isSubscribed members
  Log.logInfo "Bootstrap: members fetched" $
    Aeson.object
      [ "total" .= length members,
        "subscribed" .= length subscribed
      ]
  (inserted, skipped) <- foldInsertCounts conn subscribed
  Log.logInfo "Bootstrap: complete" $
    Aeson.object
      [ "inserted" .= inserted,
        "skipped_conflict" .= skipped
      ]

-- | Insert each member via 'upsertFromMailchimp', counting fresh inserts vs
-- conflicts. Errors crash the job because bootstrap is rare and we want
-- loud failures.
foldInsertCounts ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  [Member] ->
  m (Int, Int)
foldInsertCounts conn = go (0, 0)
  where
    go acc [] = pure acc
    go (!ins, !skip) (Member {mEmailAddress, mId} : rest) = do
      let stmt =
            Session.statement () $
              NewsletterSubscribers.upsertFromMailchimp
                mEmailAddress
                mId
                "subscribed"
      result <- liftIO $ try @SomeException $ Session.run stmt conn
      case result of
        Left e -> do
          Log.logAttention "Bootstrap upsert raised" $
            Aeson.object ["email" .= display mEmailAddress, "error" .= show e]
          liftIO exitFailure
        Right (Left sessionErr) -> do
          Log.logAttention "Bootstrap upsert session error" $
            Aeson.object
              ["email" .= display mEmailAddress, "error" .= show sessionErr]
          liftIO exitFailure
        Right (Right Nothing) -> go (ins, skip + 1) rest
        Right (Right (Just _)) -> go (ins + 1, skip) rest

--------------------------------------------------------------------------------
-- Reconcile mode

-- | Compare every Mailchimp member against every Postgres row and apply
-- the divergence rules described in the module header.
runReconcile ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  MailchimpClient ->
  m ()
runReconcile conn client = do
  Log.logInfo_ "Reconcile: fetching Mailchimp members"
  mcMembers <- fetchAllMembers client
  -- 'EmailAddress' is not in 'Ord', so we key by the displayed Text.
  let mcByEmail :: Map Text (EmailAddress, Text, MemberStatus)
      mcByEmail =
        Map.fromList
          [ (display mEmailAddress, (mEmailAddress, mId, mStatus))
          | Member {mEmailAddress, mId, mStatus} <- mcMembers
          ]
  Log.logInfo "Reconcile: Mailchimp side ready" $
    Aeson.object ["mc_members" .= Map.size mcByEmail]

  Log.logInfo_ "Reconcile: streaming Postgres subscribers"
  pgRows <- streamPgSubscribers conn
  Log.logInfo "Reconcile: Postgres side ready" $
    Aeson.object ["pg_rows" .= length pgRows]

  -- Walk PG rows: push any that are missing or stale, delete any whose MC
  -- record now reports unsubscribed or cleaned.
  let pgEmails :: Set.Set Text
      pgEmails =
        Set.fromList (map (display . NewsletterSubscribers.mEmail) pgRows)

  (pushedN, removedN, errorsN) <-
    foldPgRows conn client mcByEmail pgRows

  -- Walk MC: log MC-only memberships.
  let mcOnly =
        [ email
        | (key, (email, _, status)) <- Map.toList mcByEmail,
          status == Subscribed,
          not (Set.member key pgEmails)
        ]
  forM_ mcOnly $ \email ->
    Log.logAttention "Reconcile: MC-only signup; manual review required" $
      Aeson.object ["email" .= display email]

  Log.logInfo "Reconcile: complete" $
    Aeson.object
      [ "pulled" .= Map.size mcByEmail,
        "pushed" .= pushedN,
        "removed" .= removedN,
        "mc_only_warned" .= length mcOnly,
        "errors" .= errorsN
      ]

-- | Walk the Postgres rows and apply each divergence rule.
foldPgRows ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  MailchimpClient ->
  Map Text (EmailAddress, Text, MemberStatus) ->
  [NewsletterSubscribers.Model] ->
  m (Int, Int, Int)
foldPgRows conn client mcByEmail = go (0, 0, 0)
  where
    go acc [] = pure acc
    go (!pushed, !removed, !errors) (row : rest) =
      let email = NewsletterSubscribers.mEmail row
          subId = NewsletterSubscribers.mId row
          mcEntry = Map.lookup (display email) mcByEmail
          rowStatus = NewsletterSubscribers.mMailchimpStatus row
          rowMemberId = NewsletterSubscribers.mMailchimpMemberId row
       in case mcEntry of
            Nothing -> do
              -- PG-only: push to MC.
              didPush <- pushUpsert conn client email subId
              go
                ( if didPush then pushed + 1 else pushed,
                  removed,
                  if didPush then errors else errors + 1
                )
                rest
            Just (_, _, Unsubscribed) -> do
              -- MC says opted out: hard-delete the local row.
              didDelete <- deletePgRow conn email subId
              go
                ( pushed,
                  if didDelete then removed + 1 else removed,
                  if didDelete then errors else errors + 1
                )
                rest
            Just (_, _, Cleaned) -> do
              -- Hard bounce: same handling as Unsubscribed.
              didDelete <- deletePgRow conn email subId
              go
                ( pushed,
                  if didDelete then removed + 1 else removed,
                  if didDelete then errors else errors + 1
                )
                rest
            Just (_, mcId, _otherStatus)
              | rowStatus == Just "error"
                  || isNothing rowStatus
                  || rowMemberId /= Just mcId -> do
                  -- Sync metadata is wrong or stale: re-push to MC.
                  didPush <- pushUpsert conn client email subId
                  go
                    ( if didPush then pushed + 1 else pushed,
                      removed,
                      if didPush then errors else errors + 1
                    )
                    rest
            Just _ -> go (pushed, removed, errors) rest

-- | Run an upsert outbound and stamp the row with the response.
pushUpsert ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  MailchimpClient ->
  EmailAddress ->
  NewsletterSubscribers.Id ->
  m Bool
pushUpsert conn MailchimpClient {..} email subId = do
  result <- liftIO $ try @SomeException (mcUpsertMember email)
  case result of
    Left e -> do
      Log.logAttention "Reconcile upsert raised" $
        Aeson.object ["email" .= display email, "error" .= show e]
      _ <- liftIO $ try @SomeException $ Session.run (markErrorStmt subId) conn
      pure False
    Right (Left (MailchimpError clientErr)) -> do
      Log.logAttention "Reconcile upsert failed" $
        Aeson.object ["email" .= display email, "error" .= show clientErr]
      _ <- liftIO $ try @SomeException $ Session.run (markErrorStmt subId) conn
      pure False
    Right (Right Member {mId, mStatus}) -> do
      let stmt = markSyncedStmt subId mId (memberStatusTag mStatus)
      runResult <- liftIO $ try @SomeException $ Session.run stmt conn
      case runResult of
        Left e -> do
          Log.logAttention "Reconcile markSynced raised" $
            Aeson.object ["email" .= display email, "error" .= show e]
          pure False
        Right (Left sessionErr) -> do
          Log.logAttention "Reconcile markSynced session error" $
            Aeson.object ["email" .= display email, "error" .= show sessionErr]
          pure False
        Right (Right ()) -> do
          Log.logInfo "Reconcile pushed upsert" $
            Aeson.object ["email" .= display email]
          pure True

-- | Delete the local row.
deletePgRow ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  EmailAddress ->
  NewsletterSubscribers.Id ->
  m Bool
deletePgRow conn email subId = do
  let stmt = Session.statement () (NewsletterSubscribers.deleteById subId)
  result <- liftIO $ try @SomeException $ Session.run stmt conn
  case result of
    Left e -> do
      Log.logAttention "Reconcile delete raised" $
        Aeson.object ["email" .= display email, "error" .= show e]
      pure False
    Right (Left sessionErr) -> do
      Log.logAttention "Reconcile delete session error" $
        Aeson.object ["email" .= display email, "error" .= show sessionErr]
      pure False
    Right (Right _) -> do
      Log.logInfo "Reconcile removed PG row (Mailchimp opted out)" $
        Aeson.object ["email" .= display email]
      pure True

-- | Pull every row from @newsletter_subscribers@ in id order.
streamPgSubscribers ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  m [NewsletterSubscribers.Model]
streamPgSubscribers conn = do
  let stmt = Session.statement () NewsletterSubscribers.streamAll
  result <- liftIO $ try @SomeException $ Session.run stmt conn
  case result of
    Left e -> do
      Log.logAttention "streamAll raised" $ Aeson.object ["error" .= show e]
      liftIO exitFailure
    Right (Left sessionErr) -> do
      Log.logAttention "streamAll session error" $
        Aeson.object ["error" .= show sessionErr]
      liftIO exitFailure
    Right (Right rows) -> pure rows

--------------------------------------------------------------------------------
-- Mailchimp paging

-- | Fetch every member from the audience, paging at 1000 per call.
fetchAllMembers ::
  (MonadIO m, Log.MonadLog m) =>
  MailchimpClient ->
  m [Member]
fetchAllMembers MailchimpClient {..} = go 0 []
  where
    pageSize = 1000

    go offset acc = do
      result <- liftIO $ mcListMembers pageSize offset
      case result of
        Left (MailchimpError clientErr) -> do
          Log.logAttention "Mailchimp listMembers failed" $
            Aeson.object ["offset" .= offset, "error" .= show clientErr]
          liftIO exitFailure
        Right ListMembersResponse {lmrMembers, lmrTotalItems} -> do
          let !nextOffset = offset + length lmrMembers
              !nextAcc = acc <> lmrMembers
          when (length lmrMembers > 0) $
            Log.logInfo "Mailchimp listMembers page" $
              Aeson.object
                [ "offset" .= offset,
                  "page_size" .= length lmrMembers,
                  "total_items" .= lmrTotalItems
                ]
          if nextOffset >= lmrTotalItems || null lmrMembers
            then pure nextAcc
            else go nextOffset nextAcc

--------------------------------------------------------------------------------
-- Statement constructors (re-exposed)

-- | Wrap 'NewsletterSubscribers.markSynced' in a 'Session.statement ()'.
markSyncedStmt ::
  NewsletterSubscribers.Id ->
  Text ->
  Text ->
  Session.Session ()
markSyncedStmt subId memberId status =
  Session.statement () (NewsletterSubscribers.markSynced subId memberId status)

-- | Wrap 'NewsletterSubscribers.markError' in a 'Session.statement ()'.
markErrorStmt :: NewsletterSubscribers.Id -> Session.Session ()
markErrorStmt subId =
  Session.statement () (NewsletterSubscribers.markError subId)

