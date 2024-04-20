{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.User where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Tables.User
import Database.Utils
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import GHC.Generics
import Rel8 qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Domain

data User = User
  { userId :: Id,
    userEmail :: EmailAddress,
    userDisplayName :: DisplayName,
    userAvatarUrl :: Maybe Text,
    userIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

instance ModelParser UserF User where
  parseModel :: UserF Rel8.Result -> User
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: UserF Identity -> User
      parser UserF {..} =
        User
          { userId = coerce userFId,
            userEmail = coerce userFEmail,
            userDisplayName = coerce userFDisplayName,
            userAvatarUrl = coerce userFAvatarUrl,
            userIsAdmin = coerce userFIsAdmin
          }

instance ModelPrinter UserF (EmailAddress, Password, DisplayName, AdminStatus) where
  printModel :: (EmailAddress, Password, DisplayName, AdminStatus) -> UserF Rel8.Expr
  printModel (email, pass, DisplayName displayName, adminStatus) =
    UserF
      { userFId = Rel8.unsafeDefault,
        userFEmail = Rel8.litExpr (coerce email),
        userFPassword = Rel8.litExpr (coerce pass),
        userFDisplayName = Rel8.litExpr displayName,
        userFAvatarUrl = Rel8.litExpr Nothing,
        userFIsAdmin = Rel8.litExpr $ isAdmin adminStatus
      }
