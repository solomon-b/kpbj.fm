module Handlers.MailingList where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Has (Has)
import Data.Text.Encoding qualified as Text.Encoding
import Effects.MailingList (EmailAddress (EmailAddress))
import Effects.MailingList qualified as MailingList
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant ((:>))
import Servant qualified
import Text.Email.Validate qualified as Email
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: MailingList.EmailAddress}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type MailingListAPI =
  "mailing-list" :> "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------
-- Handler

mailingListHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => Servant.ServerT MailingListAPI m
mailingListHandler (MailingListForm e@(EmailAddress {..})) = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 emailAddress) $ Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid Email Address"}
  _pid <- MailingList.insertEmailAddress e
  Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (show emailAddress))

  -- TODO: Very hacky solution until we support htmx.
  -- TODO: Would be nice to render the splash page with a success message.
  Servant.throwError $ Servant.err301 {Servant.errHeaders = [("Location", "https://www.kpbj.fm")]}
