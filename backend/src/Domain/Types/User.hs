{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.User where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import GHC.Generics
import Rel8 qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Domain

data User = User
  { userId :: User.Id,
    userEmail :: EmailAddress,
    userDisplayName :: DisplayName,
    userAvatarUrl :: Maybe Text,
    userIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance User)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

instance ModelParser User.Model User where
  parseModel :: User.Model Rel8.Result -> User
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: User.Model Identity -> User
      parser User.Model {..} =
        User
          { userId = coerce umId,
            userEmail = EmailAddress (CI.mk (runIdentity umEmail)),
            userDisplayName = coerce umDisplayName,
            userAvatarUrl = coerce umAvatarUrl,
            userIsAdmin = coerce umIsAdmin
          }

instance ModelPrinter User.Model (EmailAddress, Password, DisplayName, AdminStatus) where
  printModel :: (EmailAddress, Password, DisplayName, AdminStatus) -> User.Model Rel8.Expr
  printModel (email, pass, DisplayName displayName, adminStatus) =
    User.Model
      { umId = Rel8.unsafeDefault,
        umEmail = Rel8.litExpr (CI.original (coerce email)),
        umPassword = Rel8.litExpr (coerce pass),
        umDisplayName = Rel8.litExpr displayName,
        umAvatarUrl = Rel8.litExpr Nothing,
        umIsAdmin = Rel8.litExpr $ isAdmin adminStatus
      }
