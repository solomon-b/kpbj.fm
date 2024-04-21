module API.MailingList where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Has (Has)
import Data.Text.Display (Display, display)
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class (MonadDB)
import Database.Queries.MailingList qualified as MailingList
import Domain.Types.Email (EmailAddress (..))
import Errors (throw401)
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified
import Text.Email.Validate qualified as Email
import Tracing (handlerSpan)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: EmailAddress}
  deriving stock (Show, Generic)
  deriving newtype (Display)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type MailingListAPI =
  "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------
-- Handler

mailingListHandler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadDB m,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  Servant.ServerT MailingListAPI m
mailingListHandler req@(MailingListForm e@(EmailAddress {..})) = do
  handlerSpan "/mailing-list" req display $ do
    unless (Email.isValid $ Text.Encoding.encodeUtf8 emailAddress) $ throw401 "Invalid Email Address"
    _pid <- MailingList.insertEmailAddress e
    Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (show emailAddress))

    -- TODO: Very hacky solution until we support htmx.
    -- TODO: Would be nice to render the splash page with a success message.
    throwM $ Servant.err301 {Servant.errHeaders = [("Location", "https://www.kpbj.fm")]}
