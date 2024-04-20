module Database.Queries.User where

--------------------------------------------------------------------------------

import Control.Monad.Error.Class (MonadError)
import Database.Class (MonadDB)
import Database.Tables.User
import Database.Utils
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User ()
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Effect

getUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadError Servant.ServerError m
  ) =>
  Id ->
  m (Maybe (UserF Rel8.Result))
getUser uid = execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid)

selectUserQuery :: Id -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFId userF ==. Rel8.litExpr uid
  pure userF

getUserByCredential ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadError Servant.ServerError m
  ) =>
  EmailAddress ->
  Password ->
  m (Maybe (UserF Rel8.Result))
getUserByCredential email pass = execQuerySpanThrowMessage "Failed to query users table" (selectUserByCredentialQuery email pass)

selectUserByCredentialQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserByCredentialQuery (EmailAddress email) (Password pass) = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFEmail userF ==. Rel8.litExpr email &&. userFPassword userF ==. Rel8.litExpr pass
  pure userF

getUserByEmail ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadError Servant.ServerError m
  ) =>
  EmailAddress ->
  m (Maybe (UserF Rel8.Result))
getUserByEmail email = execQuerySpanThrowMessage "Failed to query users table" (selectUserByEmailQuery email)

selectUserByEmailQuery :: EmailAddress -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserByEmailQuery (EmailAddress email) = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFEmail userF ==. Rel8.litExpr email
  pure userF

getUsers ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadError Servant.ServerError m
  ) =>
  m [UserF Rel8.Result]
getUsers = execQuerySpanThrowMessage "Failed to query users table" selectUsersQuery

selectUsersQuery :: HSQL.Statement () [UserF Rel8.Result]
selectUsersQuery = Rel8.run . Rel8.select $ Rel8.each userFSchema

insertUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadError Servant.ServerError m
  ) =>
  (EmailAddress, Password, DisplayName, AdminStatus) ->
  m Id
insertUser = execQuerySpanThrowMessage "Failed to insert user" . insertUserQuery

insertUserQuery :: (EmailAddress, Password, DisplayName, AdminStatus) -> HSQL.Statement () Id
insertUserQuery newUser =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { into = userFSchema,
          rows = Rel8.values [printModel newUser],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning userFId
        }
