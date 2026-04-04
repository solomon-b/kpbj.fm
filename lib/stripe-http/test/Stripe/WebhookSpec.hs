module Stripe.WebhookSpec (spec) where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC qualified as HMAC
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Stripe.Types (StripeWebhookSecret (..))
import Stripe.Webhook (VerificationError (..), verifyWebhookSignature)
import Test.Hspec


-- | Build a @Stripe-Signature@ header value from a signing secret,
-- POSIX timestamp, and raw request body.
mkSigHeader :: ByteString -> Integer -> ByteString -> Text
mkSigHeader secret timestamp body =
  let signedPayload = BS8.pack (show timestamp) <> "." <> body
      hmacDigest = HMAC.hmac secret signedPayload :: HMAC.HMAC SHA256
      hexSig = convertToBase Base16 hmacDigest :: ByteString
   in Text.pack $ "t=" <> show timestamp <> ",v1=" <> BS8.unpack hexSig


spec :: Spec
spec = describe "verifyWebhookSignature" $ do
  let secret = StripeWebhookSecret "whsec_test_secret"
      body = "{\"id\":\"evt_123\",\"type\":\"payment_intent.succeeded\"}"

  it "accepts a valid signature" $ do
    now <- round <$> getPOSIXTime :: IO Integer
    let header = mkSigHeader secret.unStripeWebhookSecret now body
    result <- verifyWebhookSignature secret header body
    result `shouldBe` Right ()

  it "rejects an invalid signature (wrong secret)" $ do
    now <- round <$> getPOSIXTime :: IO Integer
    let header = mkSigHeader "wrong_secret" now body
    result <- verifyWebhookSignature secret header body
    result `shouldBe` Left SignatureMismatch

  it "rejects a timestamp older than 5 minutes" $ do
    now <- round <$> getPOSIXTime :: IO Integer
    let oldTimestamp = now - 600
        header = mkSigHeader secret.unStripeWebhookSecret oldTimestamp body
    result <- verifyWebhookSignature secret header body
    result `shouldBe` Left TimestampTooOld

  it "rejects a malformed signature header" $ do
    result <- verifyWebhookSignature secret "garbage-not-a-real-header" body
    result `shouldBe` Left InvalidSignatureFormat
