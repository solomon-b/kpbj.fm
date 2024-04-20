module Database.Tables.MailingList where

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
data MailingListF f = MailingListF
  { mailingListId :: f Id,
    email :: f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able)

mailingListSchema :: Rel8.TableSchema (MailingListF Rel8.Name)
mailingListSchema =
  Rel8.TableSchema
    { Rel8.name = "mailing_list",
      Rel8.columns =
        MailingListF
          { mailingListId = "id",
            email = "email"
          }
    }
