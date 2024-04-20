module Database.Tables.User where

--------------------------------------------------------------------------------

import Barbies
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Rel8 qualified
import Servant qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType, ToJSON, FromJSON)
  deriving anyclass (ToJWT, FromJWT)

-- | Database Model for the `userF` table.
data UserF f = UserF
  { userFId :: f Id,
    userFEmail :: f Text,
    userFPassword :: f Text,
    userFDisplayName :: f Text,
    userFAvatarUrl :: f (Maybe Text),
    userFIsAdmin :: f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

userFSchema :: Rel8.TableSchema (UserF Rel8.Name)
userFSchema =
  Rel8.TableSchema
    { Rel8.name = "users",
      Rel8.columns =
        UserF
          { userFId = "id",
            userFEmail = "email",
            userFPassword = "password",
            userFDisplayName = "display_name",
            userFAvatarUrl = "avatar_url",
            userFIsAdmin = "is_admin"
          }
    }
