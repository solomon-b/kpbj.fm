module Test.Gen.Tables.PasswordResetTokens (mkInsert, passwordResetInsertGen) where

--------------------------------------------------------------------------------

import Control.Monad (replicateM)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.PasswordResetTokens qualified as PRT
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | Build an Insert from test setup values and generated audit fields.
mkInsert :: User.Id -> PRT.Token -> Text -> Maybe Text -> Maybe Text -> PRT.Insert
mkInsert userId token email ipAddress userAgent =
  PRT.Insert
    { iUserId = userId,
      iToken = token,
      iEmail = email,
      iIpAddress = ipAddress,
      iUserAgent = userAgent
    }

-- | Generate a realistic IPv4 address.
genIPv4 :: (MonadGen m) => m Text
genIPv4 = do
  octets <- replicateM 4 (Gen.int (Range.linear 0 255))
  pure $ Text.intercalate "." (fmap (Text.pack . show) octets)

-- | Generate random audit fields (ipAddress, userAgent).
passwordResetInsertGen :: (MonadGen m) => m (Maybe Text, Maybe Text)
passwordResetInsertGen = do
  ipAddress <- Gen.maybe genIPv4
  userAgent <- Gen.maybe $ Gen.text (Range.linear 10 50) Gen.alphaNum
  pure (ipAddress, userAgent)
