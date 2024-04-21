module Database.Queries.MailingList where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Database.Class (MonadDB)
import Database.Tables.MailingList qualified as MailingList
import Database.Utils
import Domain.Types.Email
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 qualified

--------------------------------------------------------------------------------

insertEmailAddress ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  EmailAddress ->
  m MailingList.Id
insertEmailAddress email =
  execQuerySpanThrowMessage "Failed to insert email address" $ insertEmailAddressSql email

insertEmailAddressSql :: EmailAddress -> HSQL.Statement () MailingList.Id
insertEmailAddressSql EmailAddress {..} =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { Rel8.into = MailingList.schema,
          Rel8.rows = Rel8.values [MailingList.Model Rel8.unsafeDefault (Rel8.litExpr emailAddress)],
          Rel8.onConflict = Rel8.Abort,
          Rel8.returning = Rel8.Returning MailingList.mlId
        }
