{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Effects.User where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader)
import DB.Utils (execQuerySpanThrowMessage)
import DB.Utils qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..))
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified
import Servant qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Domain

data User = User
  { userId :: Id,
    userEmail :: EmailAddress,
    userDisplayName :: DisplayName,
    userAvatarUrl :: Maybe Text,
    userIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

instance DB.Utils.ModelParser UserF User where
  parseModel :: UserF Rel8.Result -> User
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: UserF Identity -> User
      parser UserF {..} =
        User
          { userId = coerce userFId,
            userEmail = coerce userFEmail,
            userDisplayName = coerce userFDisplayName,
            userAvatarUrl = coerce userFAvatarUrl,
            userIsAdmin = coerce userFIsAdmin
          }

instance DB.Utils.ModelPrinter UserF (EmailAddress, Password, DisplayName, AdminStatus) where
  printModel :: (EmailAddress, Password, DisplayName, AdminStatus) -> UserF Rel8.Expr
  printModel (email, pass, DisplayName displayName, adminStatus) =
    UserF
      { userFId = Rel8.unsafeDefault,
        userFEmail = Rel8.litExpr (coerce email),
        userFPassword = Rel8.litExpr (coerce pass),
        userFDisplayName = Rel8.litExpr displayName,
        userFAvatarUrl = Rel8.litExpr Nothing,
        userFIsAdmin = Rel8.litExpr $ isAdmin adminStatus
      }

newtype EmailAddress = EmailAddress {emailAddress :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData, FromJSON, ToJSON)

newtype DisplayName = DisplayName {displayName :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData, FromJSON, ToJSON)

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType, ToJSON, FromJSON)
  deriving anyclass (ToJWT, FromJWT)

newtype Password = Password Text
  deriving stock (Show, Generic, Eq)
  deriving newtype (FromJSON, ToJSON)

data AdminStatus = IsAdmin | IsNotAdmin
  deriving stock (Show, Generic, Eq)

instance FromJSON AdminStatus where
  parseJSON = Aeson.withBool "IsAdmin" $ \case
    True -> pure IsAdmin
    False -> pure IsNotAdmin

isAdmin :: AdminStatus -> Bool
isAdmin = \case
  IsAdmin -> True
  IsNotAdmin -> False

--------------------------------------------------------------------------------
-- Model

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

--------------------------------------------------------------------------------
-- Effect

getUser ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  Id ->
  m (Maybe User)
getUser uid = do
  user <- execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid)
  pure $ fmap DB.Utils.parseModel user

selectUserQuery :: Id -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFId userF ==. Rel8.litExpr uid
  pure userF

getUserByCredential ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  EmailAddress ->
  Password ->
  m (Maybe User)
getUserByCredential email pass = do
  user <- execQuerySpanThrowMessage "Failed to query users table" (selectUserByCredentialQuery email pass)
  pure $ fmap DB.Utils.parseModel user

selectUserByCredentialQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserByCredentialQuery (EmailAddress email) (Password pass) = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFEmail userF ==. Rel8.litExpr email &&. userFPassword userF ==. Rel8.litExpr pass
  pure userF

getUserByEmail ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  EmailAddress ->
  m (Maybe User)
getUserByEmail email = do
  user <- execQuerySpanThrowMessage "Failed to query users table" (selectUserByEmailQuery email)
  pure $ fmap DB.Utils.parseModel user

selectUserByEmailQuery :: EmailAddress -> HSQL.Statement () (Maybe (UserF Rel8.Result))
selectUserByEmailQuery (EmailAddress email) = Rel8.runMaybe . Rel8.select $ do
  userF <- Rel8.each userFSchema
  Rel8.where_ $ userFEmail userF ==. Rel8.litExpr email
  pure userF

getUsers ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  m [User]
getUsers = do
  users <- execQuerySpanThrowMessage "Failed to query users table" selectUsersQuery
  pure $ fmap DB.Utils.parseModel users

selectUsersQuery :: HSQL.Statement () [UserF Rel8.Result]
selectUsersQuery = Rel8.run . Rel8.select $ Rel8.each userFSchema

insertUser ::
  forall env m.
  ( Log.MonadLog m,
    MonadIO m,
    MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env
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
          rows = Rel8.values [DB.Utils.printModel newUser],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning userFId
        }
