module Test.Gen.Tables.EmailVerificationTokens (mkInsert) where

--------------------------------------------------------------------------------

import Data.Text (Text)
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
