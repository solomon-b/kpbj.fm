{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Effects.MailingList where

--------------------------------------------------------------------------------

import Control.Concurrent.MVar (MVar)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Exception (Exc)
import Control.Monad.Freer.Exception qualified as Exc
import Control.Monad.Freer.Reader (Reader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
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
-- Effect

data Model a where
  InsertEmailAddress :: Text -> Model Id

--------------------------------------------------------------------------------
-- Senders

insertEmailAddress :: (Freer.Member Model r) => Text -> Freer.Eff r Id
insertEmailAddress = Freer.send . InsertEmailAddress

--------------------------------------------------------------------------------
-- Interpreter

runMailingListModel :: forall env r v. (Freer.Member (Log.LogT IO) r, Freer.Member (Exc ModelError) r, Freer.Member (Reader env) r, Has (MVar Table) env) => Freer.Eff (Model ': r) v -> Freer.Eff r v
runMailingListModel = Utils.simpleRelay $ \case
  InsertEmailAddress email -> do
    pid <- Utils.viewTable @env @Table fst
    Utils.overTable @env @Table $ fmap (Map.insert pid $ EmailAddress email)
    pure pid

type Table = (Id, Map Id EmailAddress)

emailTable :: Map Id EmailAddress
emailTable = Map.fromList [(0, EmailAddress "jones@microsoft.net"), (1, EmailAddress "tim@google.com"), (2, EmailAddress "foo@bar.baz")]
