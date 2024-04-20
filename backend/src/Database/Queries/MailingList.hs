module Database.Queries.MailingList where

--------------------------------------------------------------------------------

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Database.Tables.MailingList
import Database.Utils
import Domain.Types.Email
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------

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
  execQuerySpanThrowMessage "Failed to insert email address" $ insertEmailAddressSql email

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
