module Effects.Database.Queries.User where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Data.CaseInsensitive qualified as CI
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User ()
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified

--------------------------------------------------------------------------------

selectUserQuery :: User.Id -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umId um ==. Rel8.litExpr uid
  pure um

selectUserByCredentialQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByCredentialQuery (EmailAddress email) (Password pass) = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email) &&. User.umPassword um ==. Rel8.litExpr pass
  pure um

selectUserByEmailQuery :: EmailAddress -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByEmailQuery (EmailAddress email) = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email)
  pure um

selectUsersQuery :: HSQL.Statement () [User.Model Rel8.Result]
selectUsersQuery = Rel8.run . Rel8.select $ Rel8.each User.schema

insertUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  (EmailAddress, Password, DisplayName, AdminStatus) ->
  m User.Id
insertUser = execQuerySpanThrowMessage "Failed to insert user" . insertUserQuery

insertUserQuery :: (EmailAddress, Password, DisplayName, AdminStatus) -> HSQL.Statement () User.Id
insertUserQuery newUser =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { into = User.schema,
          rows = Rel8.values [printModel newUser],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning User.umId
        }
