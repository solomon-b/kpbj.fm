module Mailchimp.WebhookSpec (spec) where

import Data.ByteString (ByteString)
import Data.Text.Display (display)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Mailchimp.Webhook
  ( WebhookEvent (..),
    parseWebhookEvent,
    validateWebhookKey,
  )
import Mailchimp.Config (MailchimpWebhookSecret (..))
import Test.Hspec

--------------------------------------------------------------------------------

-- | A reference @fired_at@ used in the example payloads.
firedAt :: UTCTime
firedAt = UTCTime (fromGregorian 2026 5 6) (secondsToDiffTime (12 * 3600 + 34 * 60 + 56))

-- | URL-encoded form body for an event with the given @type@ and @data[email]@.
mkBody :: ByteString -> ByteString -> ByteString
mkBody eventType email =
  "type=" <> eventType
    <> "&fired_at=2026-05-06+12%3A34%3A56"
    <> "&data%5Bemail%5D="
    <> email

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseWebhookEvent" $ do
    describe "valid payloads" $ do
      it "parses a subscribe event" $ do
        let body = mkBody "subscribe" "alice%40example.com"
        case parseWebhookEvent body of
          Right (Subscribe email t) -> do
            display email `shouldBe` "alice@example.com"
            t `shouldBe` firedAt
          other -> expectationFailure $ "expected Subscribe, got " <> show other

      it "parses an unsubscribe event" $ do
        let body = mkBody "unsubscribe" "bob%40example.com"
        case parseWebhookEvent body of
          Right (Unsubscribe email t) -> do
            display email `shouldBe` "bob@example.com"
            t `shouldBe` firedAt
          other -> expectationFailure $ "expected Unsubscribe, got " <> show other

      it "parses a cleaned event" $ do
        let body = mkBody "cleaned" "carol%40example.com"
        case parseWebhookEvent body of
          Right (Cleaned email t) -> do
            display email `shouldBe` "carol@example.com"
            t `shouldBe` firedAt
          other -> expectationFailure $ "expected Cleaned, got " <> show other

      it "parses an upemail event with old and new addresses" $ do
        let body =
              "type=upemail"
                <> "&fired_at=2026-05-06+12%3A34%3A56"
                <> "&data%5Bold_email%5D=old%40example.com"
                <> "&data%5Bnew_email%5D=new%40example.com"
        case parseWebhookEvent body of
          Right (EmailUpdate oldE newE t) -> do
            display oldE `shouldBe` "old@example.com"
            display newE `shouldBe` "new@example.com"
            t `shouldBe` firedAt
          other -> expectationFailure $ "expected EmailUpdate, got " <> show other

      it "collapses unknown event types to OtherEvent" $ do
        let body =
              "type=campaign"
                <> "&fired_at=2026-05-06+12%3A34%3A56"
                <> "&data%5Bid%5D=abc"
        parseWebhookEvent body `shouldBe` Right (OtherEvent "campaign")

    describe "malformed payloads" $ do
      it "rejects a missing type field" $ do
        let body = "fired_at=2026-05-06+12%3A34%3A56"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

      it "rejects a missing fired_at field" $ do
        let body = "type=unsubscribe&data%5Bemail%5D=x%40example.com"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

      it "rejects a missing data[email] for unsubscribe" $ do
        let body = "type=unsubscribe&fired_at=2026-05-06+12%3A34%3A56"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

      it "rejects a malformed fired_at" $ do
        let body =
              "type=unsubscribe"
                <> "&fired_at=not-a-timestamp"
                <> "&data%5Bemail%5D=x%40example.com"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

      it "rejects an invalid email payload" $ do
        let body = mkBody "unsubscribe" "not-an-email"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

      it "rejects an upemail with one missing address" $ do
        let body =
              "type=upemail"
                <> "&fired_at=2026-05-06+12%3A34%3A56"
                <> "&data%5Bold_email%5D=old%40example.com"
        case parseWebhookEvent body of
          Left _ -> pure ()
          Right ev ->
            expectationFailure $ "expected Left, got " <> show ev

  describe "validateWebhookKey" $ do
    let secret = MailchimpWebhookSecret "s3cret-key"

    it "accepts a matching key" $
      validateWebhookKey secret (Just "s3cret-key") `shouldBe` True

    it "rejects a mismatched key" $
      validateWebhookKey secret (Just "wrong") `shouldBe` False

    it "rejects a missing key (Nothing)" $
      validateWebhookKey secret Nothing `shouldBe` False

    it "rejects an empty-string key" $
      validateWebhookKey secret (Just "") `shouldBe` False
