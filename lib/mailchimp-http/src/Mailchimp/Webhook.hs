-- | Mailchimp webhook event parsing and authentication.
--
-- Mailchimp authenticates webhook deliveries by matching the @?key=<secret>@
-- query parameter we register the URL with. There is no HMAC signature.
-- Bodies are @application/x-www-form-urlencoded@ with bracket-indexed keys
-- like @data[email]@.
module Mailchimp.Webhook
  ( -- * Events
    WebhookEvent (..),
    parseWebhookEvent,

    -- * Authentication
    validateWebhookKey,
  )
where

--------------------------------------------------------------------------------

import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Domain.Types.EmailAddress (EmailAddress, mkEmailAddress, validate)
import Mailchimp.Config (MailchimpWebhookSecret (..))
import Network.HTTP.Types.URI (parseQuery)

--------------------------------------------------------------------------------
-- Events

-- | A parsed Mailchimp webhook event.
--
-- 'OtherEvent' captures any event type we have not modeled explicitly so
-- the receiver can log and acknowledge them without crashing.
data WebhookEvent
  = -- | A subscriber opted out via the Mailchimp unsubscribe link.
    Unsubscribe
      { weEmail :: EmailAddress,
        weFiredAt :: UTCTime
      }
  | -- | Mailchimp removed an address after a hard bounce.
    Cleaned
      { weEmail :: EmailAddress,
        weFiredAt :: UTCTime
      }
  | -- | A subscriber updated their email address inside Mailchimp.
    EmailUpdate
      { weOldEmail :: EmailAddress,
        weNewEmail :: EmailAddress,
        weFiredAt :: UTCTime
      }
  | -- | A subscriber signed up via a Mailchimp form. Logged-only on our side
    -- since our own signup paths already write to Postgres first.
    Subscribe
      { weEmail :: EmailAddress,
        weFiredAt :: UTCTime
      }
  | -- | An event type we have not modeled (for example @profile@). The
    -- receiver logs and acks these without acting on them.
    OtherEvent Text
  deriving stock (Show, Eq)

-- | Parse a webhook payload into a 'WebhookEvent'.
--
-- Recognizes @subscribe@, @unsubscribe@, @cleaned@, and @upemail@ event
-- types. Anything else collapses to 'OtherEvent'. Returns 'Left' when
-- required fields are missing or malformed.
parseWebhookEvent :: ByteString -> Either Text WebhookEvent
parseWebhookEvent body = do
  let pairs = parseQuery body
  eventType <- requireField pairs "type"
  case eventType of
    "subscribe" -> do
      email <- requireEmail pairs "data[email]"
      firedAt <- requireFiredAt pairs
      pure $ Subscribe email firedAt
    "unsubscribe" -> do
      email <- requireEmail pairs "data[email]"
      firedAt <- requireFiredAt pairs
      pure $ Unsubscribe email firedAt
    "cleaned" -> do
      email <- requireEmail pairs "data[email]"
      firedAt <- requireFiredAt pairs
      pure $ Cleaned email firedAt
    "upemail" -> do
      oldEmail <- requireEmail pairs "data[old_email]"
      newEmail <- requireEmail pairs "data[new_email]"
      firedAt <- requireFiredAt pairs
      pure $ EmailUpdate oldEmail newEmail firedAt
    other -> pure $ OtherEvent (Text.Encoding.decodeUtf8 other)

--------------------------------------------------------------------------------
-- Authentication

-- | Constant-time comparison of the configured webhook secret against the
-- @?key=@ query parameter on the inbound request.
--
-- Returns 'False' when the request omitted the key entirely.
validateWebhookKey :: MailchimpWebhookSecret -> Maybe Text -> Bool
validateWebhookKey (MailchimpWebhookSecret expected) = \case
  Nothing -> False
  Just provided ->
    constEq
      (Text.Encoding.encodeUtf8 expected)
      (Text.Encoding.encodeUtf8 provided)

--------------------------------------------------------------------------------
-- Internal helpers

-- | Look up a required field in the parsed query, returning the raw bytes.
requireField :: [(ByteString, Maybe ByteString)] -> ByteString -> Either Text ByteString
requireField pairs key =
  case lookup key pairs of
    Just (Just v) | not (BS.null v) -> Right v
    _ -> Left $ "Missing required field: " <> Text.Encoding.decodeUtf8 key

-- | Look up an email field, lift it into an 'EmailAddress', and validate it.
--
-- 'mkEmailAddress' is total and accepts any 'Text', so we run 'validate' to
-- reject malformed payloads here. Without this check, an invalid 'upemail'
-- @data[new_email]@ would slip through to the outbound 'Upsert', the
-- Mailchimp API would 400, and the row would be left at
-- @mailchimp_status='error'@ for no actionable reason.
requireEmail :: [(ByteString, Maybe ByteString)] -> ByteString -> Either Text EmailAddress
requireEmail pairs key = do
  raw <- requireField pairs key
  let candidate = mkEmailAddress (Text.Encoding.decodeUtf8 raw)
  case validate candidate of
    Right valid -> Right valid
    Left _ ->
      Left $
        "Invalid email in webhook payload at "
          <> Text.Encoding.decodeUtf8 key
          <> ": "
          <> Text.Encoding.decodeUtf8 raw

-- | Look up the @fired_at@ field and parse it as @YYYY-MM-DD HH:MM:SS@.
--
-- Mailchimp formats timestamps in UTC without an offset suffix.
requireFiredAt :: [(ByteString, Maybe ByteString)] -> Either Text UTCTime
requireFiredAt pairs = do
  raw <- requireField pairs "fired_at"
  let asString = Text.unpack (Text.Encoding.decodeUtf8 raw)
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" asString of
    Just t -> Right t
    Nothing -> Left $ "Could not parse fired_at: " <> Text.Encoding.decodeUtf8 raw
