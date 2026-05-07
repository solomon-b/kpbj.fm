-- | Mailchimp configuration newtypes.
--
-- The credentials carried here are loaded once at app startup. Each newtype
-- has a censored 'Show' instance so the values never appear in logs.
module Mailchimp.Config
  ( -- * Credentials
    MailchimpApiKey (..),
    MailchimpAudienceId (..),
    MailchimpWebhookSecret (..),

    -- * Helpers
    parseDataCenter,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

--------------------------------------------------------------------------------
-- Credentials

-- | A Mailchimp Marketing API key.
--
-- Mailchimp keys are formatted @<random>-<dc>@, where @<dc>@ is the data
-- center suffix (for example @us21@). The 'Show' instance is censored so
-- credentials are not leaked through logs.
newtype MailchimpApiKey = MailchimpApiKey {unMailchimpApiKey :: ByteString}
  deriving newtype (Eq)

instance Show MailchimpApiKey where
  show _ = "MailchimpApiKey \"<redacted>\""

-- | A Mailchimp audience (list) identifier.
--
-- Audience ids are short hex strings. They are not secret on their own but
-- are kept opaque here for typing discipline.
newtype MailchimpAudienceId = MailchimpAudienceId {unMailchimpAudienceId :: Text}
  deriving newtype (Eq, Show)

-- | The shared secret embedded in the registered Mailchimp webhook URL.
--
-- Mailchimp authenticates webhook deliveries by the @?key=<secret>@ query
-- parameter we register with the URL. The 'Show' instance is censored.
newtype MailchimpWebhookSecret = MailchimpWebhookSecret {unMailchimpWebhookSecret :: Text}
  deriving newtype (Eq)

instance Show MailchimpWebhookSecret where
  show _ = "MailchimpWebhookSecret \"<redacted>\""

--------------------------------------------------------------------------------
-- Helpers

-- | Extract the data center suffix from a Mailchimp API key.
--
-- Mailchimp keys end in @-<dc>@, where the random portion is hex and the
-- data center is alphanumeric (for example @us21@, @us6@). We split on the
-- LAST dash so any random portion that happens to contain dashes is
-- preserved. Whitespace from environment-file accidents is stripped before
-- parsing.
--
-- Rejects keys that have:
--
--   * no dash at all,
--   * an empty random portion (key starts with @-@),
--   * an empty data center suffix (key ends with @-@), or
--   * a non-alphanumeric data center suffix.
parseDataCenter :: MailchimpApiKey -> Either Text Text
parseDataCenter (MailchimpApiKey raw) =
  let asText = Text.strip (Text.Encoding.decodeUtf8 raw)
   in case Text.breakOnEnd "-" asText of
        ("", _) -> Left "Mailchimp API key missing data center suffix (no dash found)"
        (prefix, suffix)
          -- breakOnEnd returns the dash with the prefix, so a length-1 prefix
          -- of "-" means the random portion was empty.
          | Text.length prefix < 2 ->
              Left "Mailchimp API key missing random portion before data center suffix"
          | Text.null suffix ->
              Left "Mailchimp API key missing data center suffix"
          | not (Text.all isAlphaNum suffix) ->
              Left "Mailchimp API key data center suffix is malformed (non-alphanumeric)"
          | otherwise -> Right suffix
