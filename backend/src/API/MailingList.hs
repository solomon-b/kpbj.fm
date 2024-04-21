module API.MailingList where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class (MonadDB)
import Database.Queries.MailingList qualified as MailingList
import Domain.Types.Email (EmailAddress (..))
import GHC.Generics (Generic)
import Log qualified
import Servant ((:>))
import Servant qualified
import Text.Email.Validate qualified as Email
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: EmailAddress}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type MailingListAPI =
  "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------
-- Handler

mailingListHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  Servant.ServerT MailingListAPI m
mailingListHandler (MailingListForm e@(EmailAddress {..})) = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 emailAddress) $ throwM $ Servant.err401 {Servant.errBody = "Invalid Email Address"}
  _pid <- MailingList.insertEmailAddress e
  Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (show emailAddress))

  -- TODO: Very hacky solution until we support htmx.
  -- TODO: Would be nice to render the splash page with a success message.
  throwM $ Servant.err301 {Servant.errHeaders = [("Location", "https://www.kpbj.fm")]}
