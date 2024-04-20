module Effects.MailingList where

--------------------------------------------------------------------------------

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import DB.Utils qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Domain

-- TODO: Use case-insensitive:
newtype EmailAddress = EmailAddress {emailAddress :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData)
  deriving anyclass (FromJSON, ToJSON)

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Effect

insertEmailAddress ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  EmailAddress ->
  m Id
insertEmailAddress email =
  DB.Utils.execQuerySpanThrowMessage "Failed to insert email address" $ insertEmailAddressSql email

insertEmailAddressSql :: EmailAddress -> HSQL.Statement () Id
insertEmailAddressSql EmailAddress {..} =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { Rel8.into = mailingListSchema,
          Rel8.rows = Rel8.values [MailingListF Rel8.unsafeDefault (Rel8.litExpr emailAddress)],
          Rel8.onConflict = Rel8.Abort,
          Rel8.returning = Rel8.Returning mailingListId
        }

--------------------------------------------------------------------------------
-- Database

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
