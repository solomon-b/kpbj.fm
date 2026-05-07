-- | Handler for @POST /api/webhooks/mailchimp@.
--
-- Receives Mailchimp webhook deliveries (@unsubscribe@, @cleaned@,
-- @upemail@, and @subscribe@), authenticates by the @?key=@ query param,
-- and reconciles the event locally.
--
-- Always 204s on processed-or-acknowledged events. Bad authentication is
-- the only case that returns 4xx — Mailchimp will retry non-2xx responses,
-- and we don't want a malformed-but-authenticated body to cause a retry
-- storm.
module API.Webhooks.Mailchimp.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Mailchimp.Sync (SyncOp (..), syncAsync)
import Log qualified
import Mailchimp.Config (MailchimpWebhookSecret)
import Mailchimp.Webhook (WebhookEvent (..), parseWebhookEvent, validateWebhookKey)
import Servant qualified

--------------------------------------------------------------------------------

-- | Receive and process a Mailchimp webhook event.
--
-- Steps:
--
--   1. If no 'MailchimpWebhookSecret' is configured (dev / unconfigured),
--      log and return 204 — signals to Mailchimp that the URL is alive
--      without claiming we processed anything.
--   2. Validate the @?key=@ query parameter. Mismatch → 401 (Mailchimp
--      retries; we want to know about it).
--   3. Parse the form body. Failure → log + 204. Mailchimp would retry on
--      a non-2xx, but the body is malformed permanently, so retrying buys
--      us nothing.
--   4. Dispatch on event type. All branches log; only 'EmailUpdate' fires
--      a follow-up Mailchimp upsert against the new address.
handler ::
  Maybe Text ->
  ByteString ->
  AppM Servant.NoContent
handler mProvidedKey rawBody = do
  mSecret <- asks (Has.getter @(Maybe MailchimpWebhookSecret))
  case mSecret of
    Nothing -> do
      Log.logInfo_ "Mailchimp webhook received but no secret configured — acknowledging without processing"
      pure Servant.NoContent
    Just secret ->
      if not (validateWebhookKey secret mProvidedKey)
        then do
          Log.logAttention_ "Mailchimp webhook key validation failed"
          throwM Servant.err401
        else case parseWebhookEvent rawBody of
          Left err -> do
            Log.logAttention "Mailchimp webhook body could not be parsed" err
            pure Servant.NoContent
          Right event -> do
            handleEvent event
            pure Servant.NoContent

--------------------------------------------------------------------------------
-- Event dispatch

-- | Apply a parsed 'WebhookEvent' to local state.
handleEvent :: WebhookEvent -> AppM ()
handleEvent = \case
  Unsubscribe email _firedAt -> deleteByEmail "unsubscribe" email
  Cleaned email _firedAt -> tombstoneByEmail email
  EmailUpdate oldEmail newEmail _firedAt -> handleEmailUpdate oldEmail newEmail
  Subscribe email _firedAt ->
    Log.logInfo "Mailchimp subscribe event received (no-op; outbound is authoritative)" (display email)
  OtherEvent eventType ->
    Log.logInfo "Mailchimp webhook event ignored" eventType

-- | Look up the row by email and delete it.
--
-- Used for unsubscribe events. Mailchimp is authoritative on opt-outs, so a
-- hard delete is the correct local action — the user can re-subscribe later
-- via the form and Mailchimp accepts re-adding unsubscribed members.
deleteByEmail :: Text -> EmailAddress -> AppM ()
deleteByEmail label email = do
  result <- execQuery (NewsletterSubscribers.getByEmail email)
  case result of
    Left dbErr ->
      Log.logAttention "Mailchimp webhook lookup failed" (label, display email, show dbErr)
    Right Nothing ->
      Log.logInfo "Mailchimp webhook event for unknown email" (label, display email)
    Right (Just sub) -> do
      delResult <- execQuery (NewsletterSubscribers.deleteById (NewsletterSubscribers.mId sub))
      case delResult of
        Left dbErr ->
          Log.logAttention "Mailchimp webhook delete failed" (label, display email, show dbErr)
        Right _ ->
          Log.logInfo "Mailchimp webhook removed subscriber" (label, display email)

-- | Look up the row by email and mark it as cleaned (do not delete).
--
-- Cleaned addresses cannot be re-subscribed via the Mailchimp API. Deleting
-- the row would let a future homepage signup re-insert the email and fire a
-- fresh outbound upsert, which Mailchimp would reject — leaving the row
-- stuck at @mailchimp_status='error'@. Tombstoning the row instead means
-- future signups hit @ON CONFLICT DO NOTHING@, no MC call fires, and the
-- admin dashboard surfaces the cleaned status. Only admin-initiated deletion
-- clears the tombstone.
tombstoneByEmail :: EmailAddress -> AppM ()
tombstoneByEmail email = do
  result <- execQuery (NewsletterSubscribers.getByEmail email)
  case result of
    Left dbErr ->
      Log.logAttention "Mailchimp webhook lookup failed" ("cleaned" :: Text, display email, show dbErr)
    Right Nothing ->
      Log.logInfo "Mailchimp webhook event for unknown email" ("cleaned" :: Text, display email)
    Right (Just sub) -> do
      markResult <- execQuery (NewsletterSubscribers.markCleaned (NewsletterSubscribers.mId sub))
      case markResult of
        Left dbErr ->
          Log.logAttention "Mailchimp webhook markCleaned failed" (display email, show dbErr)
        Right () ->
          Log.logInfo "Mailchimp webhook tombstoned subscriber as cleaned" (display email)

-- | Handle an @upemail@ event by mirroring the address change locally and
-- pushing a fresh upsert against the new subscriber hash.
handleEmailUpdate ::
  EmailAddress ->
  EmailAddress ->
  AppM ()
handleEmailUpdate oldEmail newEmail = do
  result <- execQuery (NewsletterSubscribers.getByEmail oldEmail)
  case result of
    Left dbErr ->
      Log.logAttention "Mailchimp upemail lookup failed" (display oldEmail, show dbErr)
    Right Nothing ->
      Log.logInfo "Mailchimp upemail event for unknown email" (display oldEmail)
    Right (Just sub) -> do
      let subId = NewsletterSubscribers.mId sub
      updateResult <- execQuery (NewsletterSubscribers.updateEmail subId newEmail)
      case updateResult of
        Left dbErr ->
          Log.logAttention
            "Mailchimp upemail update failed"
            (display oldEmail, display newEmail, show dbErr)
        Right () -> do
          Log.logInfo
            "Mailchimp upemail applied"
            (display oldEmail, display newEmail)
          syncAsync (Upsert newEmail subId)
