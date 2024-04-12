module Handlers.MailingList where

--------------------------------------------------------------------------------

import Control.Monad.Freer (Eff)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Exception (Exc)
import Control.Monad.Freer.Exception qualified as Exc
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Effects.Logger (Logger)
import Effects.Logger qualified as Logger
import Effects.MailingList qualified as MailingList
import GHC.Generics (Generic)
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

mailingListHandler :: (Freer.Member (Exc Servant.ServerError) r, Freer.Member MailingList.Model r, Freer.Member Logger r) => Servant.ServerT MailingListAPI (Eff r)
mailingListHandler MailingListForm {..} = do
  pid <- MailingList.insertEmailAddress emailAddress
  Logger.logInfo "Submited Email Address:" (KeyMap.singleton "email" (show emailAddress))

  -- TODO: Very hacky solution until we support htmx:
  Exc.throwError $ Servant.err301 {Servant.errHeaders = [("Location", "https://www.kpbj.fm")]}
