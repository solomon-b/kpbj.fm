module Test.Gen.Tables.EmailVerificationTokens (mkInsert, generateVerificationToken) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Effects.Database.Tables.EmailVerificationTokens qualified as EVT
import Effects.Database.Tables.User qualified as User

--------------------------------------------------------------------------------

-- | Build an Insert from test setup values.
mkInsert :: User.Id -> EVT.Token -> Text -> EVT.Insert
mkInsert userId token email =
  EVT.Insert
    { iUserId = userId,
      iToken = token,
      iEmail = email
    }

-- | Generate a cryptographically secure verification token for tests.
generateVerificationToken :: (MonadIO m) => m EVT.Token
generateVerificationToken = liftIO $ do
  uuid <- UUID.V4.nextRandom
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ EVT.Token tokenText
