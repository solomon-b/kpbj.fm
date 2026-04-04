-- | Stripe webhook signature verification.
--
-- Stripe signs every webhook payload with an HMAC-SHA256 signature.
-- This module verifies those signatures using timing-safe comparison
-- to prevent both replay attacks and timing attacks.
--
-- See https://docs.stripe.com/webhooks/signatures
module Stripe.Webhook
  ( -- * Verification
    verifyWebhookSignature,

    -- * Errors
    VerificationError (..),
  )
where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC qualified as HMAC
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Stripe.Types (StripeWebhookSecret (..))

-- | Errors that can occur during webhook signature verification.
data VerificationError
  = -- | The @Stripe-Signature@ header is missing or empty.
    MissingSignatureHeader
  | -- | The header value could not be parsed into timestamp and signature components.
    InvalidSignatureFormat
  | -- | The signature timestamp is more than 5 minutes old, indicating a
    -- possible replay attack.
    TimestampTooOld
  | -- | The computed HMAC does not match the provided signature.
    SignatureMismatch
  deriving stock (Show, Eq)

-- | Maximum allowed age for a webhook signature, in seconds.
maxTimestampAge :: Integer
maxTimestampAge = 300

-- | Verify a Stripe webhook signature.
--
-- Parses the @Stripe-Signature@ header, checks the timestamp is recent,
-- computes the expected HMAC-SHA256, and performs a timing-safe comparison.
--
-- The comparison is performed on hex-encoded strings using 'constEq' to
-- prevent timing attacks.
--
-- ==== Parameters
--
-- * @secret@ — The webhook endpoint's signing secret (starts with @whsec_@).
-- * @signatureHeader@ — The full value of the @Stripe-Signature@ HTTP header.
-- * @body@ — The raw, unparsed request body.
verifyWebhookSignature ::
  -- | Webhook signing secret
  StripeWebhookSecret ->
  -- | Stripe-Signature header value
  Text ->
  -- | Raw request body
  ByteString ->
  IO (Either VerificationError ())
verifyWebhookSignature secret signatureHeader body =
  case parseSignatureHeader signatureHeader of
    Nothing -> pure $ Left InvalidSignatureFormat
    Just (timestamp, signatureHex) -> do
      now <- getPOSIXTime
      let age = round now - timestamp
      if age > maxTimestampAge
        then pure $ Left TimestampTooOld
        else
          let expectedHex = computeSignatureHex secret timestamp body
           in if constEq expectedHex signatureHex
                then pure $ Right ()
                else pure $ Left SignatureMismatch

-- | Parse the @Stripe-Signature@ header into timestamp and v1 signature.
--
-- The header format is: @t=TIMESTAMP,v1=SIGNATURE[,v1=SIGNATURE...]@
-- We extract the first @t=@ and @v1=@ values. The signature is returned
-- as the raw hex-encoded bytes from the header (not decoded).
parseSignatureHeader :: Text -> Maybe (Integer, ByteString)
parseSignatureHeader header =
  let pairs = map (Text.breakOn "=") (Text.splitOn "," header)
      findValue key = lookup key [(k, Text.drop 1 v) | (k, v) <- pairs]
   in do
        timestampText <- findValue "t"
        signatureText <- findValue "v1"
        timestamp <- readMaybe (Text.unpack timestampText)
        pure (timestamp, Text.encodeUtf8 signatureText)
  where
    readMaybe :: String -> Maybe Integer
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

-- | Compute the expected HMAC-SHA256 signature and return it hex-encoded.
--
-- The signed payload is @TIMESTAMP.BODY@ where @TIMESTAMP@ is the decimal
-- integer from the @Stripe-Signature@ header.
computeSignatureHex :: StripeWebhookSecret -> Integer -> ByteString -> ByteString
computeSignatureHex (StripeWebhookSecret secret) timestamp body =
  let signed = BS.concat [Text.encodeUtf8 (Text.pack (show timestamp)), ".", body]
      hmacDigest = HMAC.hmac secret signed :: HMAC.HMAC SHA256
   in convertToBase Base16 hmacDigest
