module API.User.Logout.Get.Handler where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Errors (InternalServerError (..), throwErr)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.ServerSessions qualified as Session
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Auth.Authz ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        (Lucid.Html ())
    )
handler Auth.Authz {..} = do
  env <- asks (Has.getter @Environment)
  Auth.expireServerSession (Session.dSessionId $ Session.toDomain authzSession) >>= \case
    Left err -> do
      throwErr $ InternalServerError $ Text.pack $ show err
    Right _ -> do
      let expireCookie = Auth.mkCookieSessionExpired env (Domains.cookieDomainMaybe env)
      pure $ Servant.addHeader expireCookie $ Servant.addHeader "/" (redirectTemplate "/")
