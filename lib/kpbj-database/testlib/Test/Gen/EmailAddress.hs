module Test.Gen.EmailAddress where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable
import Data.Text (Text, toLower)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Domain.Types.EmailAddress
import GHC.Stack (HasCallStack)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | Build an 'EmailAddress' from text that a test knows is valid.
--
-- Calls 'error' on invalid text, so a bad fixture stops the test at the
-- line that wrote it.
mkEmailAddressUnsafe :: (HasCallStack) => Text -> EmailAddress
mkEmailAddressUnsafe raw =
  case mkEmailAddress raw of
    Right email -> email
    Left _ -> error $ "Invalid test EmailAddress: " <> Text.unpack raw

genEmail :: (MonadGen m) => m EmailAddress
genEmail = do
  username <- Gen.text (Range.linear 1 10) Gen.alphaNum
  domain <- Gen.text (Range.linear 1 10) Gen.alpha
  tld <- Gen.text (Range.linear 1 10) Gen.alpha
  pure . mkEmailAddressUnsafe . toLower $
    fold
      [ username,
        "@",
        domain,
        ".",
        tld
      ]

-- | Generate a guaranteed-unique 'EmailAddress' for property tests.
--
-- 'genEmail' can shrink to identical values when called more than once in a
-- single property, causing @users_email_key@ unique-constraint violations.
-- Use this helper in any test that inserts multiple users to guarantee
-- distinct emails regardless of Hedgehog shrinking.
mkUniqueEmail :: (MonadIO m) => m EmailAddress
mkUniqueEmail = liftIO $ do
  uuid <- UUID.V4.nextRandom
  pure $ mkEmailAddressUnsafe $ "test-" <> Text.pack (UUID.toString uuid) <> "@test.example.com"
