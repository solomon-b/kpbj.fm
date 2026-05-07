module Mailchimp.ConfigSpec (spec) where

import Mailchimp.Config (MailchimpApiKey (..), parseDataCenter)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "parseDataCenter" $ do
  it "extracts a typical data center suffix" $
    parseDataCenter (MailchimpApiKey "0123456789abcdef-us21")
      `shouldBe` Right "us21"

  it "extracts a short data center suffix" $
    parseDataCenter (MailchimpApiKey "deadbeef-us6")
      `shouldBe` Right "us6"

  it "strips surrounding whitespace" $
    parseDataCenter (MailchimpApiKey "  deadbeef-us21\n")
      `shouldBe` Right "us21"

  it "rejects a key with no dash" $ do
    case parseDataCenter (MailchimpApiKey "nodashhere") of
      Left _ -> pure ()
      Right dc -> expectationFailure $ "expected Left, got " <> show dc

  it "rejects a key whose random portion is empty" $ do
    case parseDataCenter (MailchimpApiKey "-us21") of
      Left _ -> pure ()
      Right dc -> expectationFailure $ "expected Left, got " <> show dc

  it "rejects a key whose suffix is empty" $ do
    case parseDataCenter (MailchimpApiKey "deadbeef-") of
      Left _ -> pure ()
      Right dc -> expectationFailure $ "expected Left, got " <> show dc

  it "rejects a key with a non-alphanumeric suffix" $ do
    case parseDataCenter (MailchimpApiKey "deadbeef-us 21") of
      Left _ -> pure ()
      Right dc -> expectationFailure $ "expected Left, got " <> show dc
