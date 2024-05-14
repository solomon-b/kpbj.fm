module Effects.Database.Tables.User where

--------------------------------------------------------------------------------

import Barbies
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display)
import GHC.Generics
import Rel8 qualified
import Servant qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType, ToJSON, FromJSON, Display)
  deriving anyclass (ToJWT, FromJWT)

-- | Database Model for the `user` table.
data Model f = Model
  { umId :: f Id,
    umEmail :: f Text,
    umPassword :: f Text,
    umDisplayName :: f Text,
    umAvatarUrl :: f (Maybe Text),
    umIsAdmin :: f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

schema :: Rel8.TableSchema (Model Rel8.Name)
schema =
  Rel8.TableSchema
    { Rel8.name = "users",
      Rel8.columns =
        Model
          { umId = "id",
            umEmail = "email",
            umPassword = "password",
            umDisplayName = "display_name",
            umAvatarUrl = "avatar_url",
            umIsAdmin = "is_admin"
          }
    }
