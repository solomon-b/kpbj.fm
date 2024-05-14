module Effects.Database.Tables.MailingList where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType)
  deriving anyclass (ToJSON, FromJSON)

-- | Database Model for the `mailing_list` table.
data Model f = Model
  { mlId :: f Id,
    mlEmail :: f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able)

schema :: Rel8.TableSchema (Model Rel8.Name)
schema =
  Rel8.TableSchema
    { Rel8.name = "mailing_list",
      Rel8.columns =
        Model
          { mlId = "id",
            mlEmail = "email"
          }
    }
