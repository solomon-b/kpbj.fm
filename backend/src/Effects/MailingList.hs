{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Effects.MailingList where

--------------------------------------------------------------------------------

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Encoding
import GHC.Generics (Generic)
import Log qualified
import Servant qualified
import Utils qualified

--------------------------------------------------------------------------------

insertEmailAddress' :: forall env m. (MonadReader env m, Has (MVar Table) env, Log.MonadLog m, MonadError Servant.ServerError m, MonadIO m) => Text -> m Id
insertEmailAddress' emailAddress = do
  pid <- Utils.viewTable @env @Table fst
  Utils.overTable @env @Table $ fmap (Map.insert pid $ EmailAddress emailAddress)
  pure pid

--------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress {emailAddress :: Text}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

newtype Id = Id Int
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData)
  deriving anyclass (ToJSON, FromJSON)

newtype ModelError = DuplicateEmail Text
  deriving stock (Show)

instance Utils.ToServerError ModelError where
  convertToServerError :: ModelError -> Servant.ServerError
  convertToServerError (DuplicateEmail email) =
    let email' = Text.Encoding.encodeUtf8 $ Text.Lazy.pack $ show email
     in Servant.err400 {Servant.errBody = "Duplicate email address: '" <> email' <> "'"}

--------------------------------------------------------------------------------

type Table = (Id, Map Id EmailAddress)

emailTable :: Map Id EmailAddress
emailTable = Map.fromList [(0, EmailAddress "jones@microsoft.net"), (1, EmailAddress "tim@google.com"), (2, EmailAddress "foo@bar.baz")]
