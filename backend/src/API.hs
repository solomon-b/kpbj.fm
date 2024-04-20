module API where

--------------------------------------------------------------------------------

import API.MailingList
import API.SplashPage
import API.User
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant (ServerError, (:<|>) (..), (:>))
import Servant qualified
import Servant.Auth.Server qualified as Servant.Auth

--------------------------------------------------------------------------------

type API =
  SplashPageAPI
    :<|> "mailing-list" :> MailingListAPI
    :<|> "user" :> UserAPI

server ::
  ( MonadError Servant.ServerError m,
    MonadReader env m,
    Has HSQL.Pool env,
    Has Servant.Auth.JWTSettings env,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Servant.ServerT API m
server = splashPageHandler :<|> mailingListHandler :<|> userHandler
