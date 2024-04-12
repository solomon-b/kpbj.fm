module Handlers.MailingList where

--------------------------------------------------------------------------------

import Control.Concurrent (MVar)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Has (Has)
import Data.Text (Text)
import Effects.MailingList qualified as MailingList
import GHC.Generics (Generic)
import Log qualified
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type MailingListAPI =
  "mailing-list" :> "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------
-- Handler

mailingListHandler :: (MonadReader env m, Has (MVar MailingList.Table) env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => Servant.ServerT MailingListAPI m
mailingListHandler MailingListForm {..} = do
  pid <- MailingList.insertEmailAddress' emailAddress
  Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (show emailAddress))

  -- TODO: Very hacky solution until we support htmx:
  Servant.throwError $ Servant.err301 {Servant.errHeaders = [("Location", "https://www.kpbj.fm")]}
