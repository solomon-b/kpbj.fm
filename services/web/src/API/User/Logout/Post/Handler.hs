{-# LANGUAGE ViewPatterns #-}

module API.User.Logout.Post.Handler where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        (Lucid.Html ())
    )
handler (coerce -> cookie) = do
  env <- asks (Has.getter @Environment)
  -- Try to expire the session if we have a valid cookie
  case cookie >>= Auth.lookupSessionId env of
    Nothing -> pure ()
    Just sessionId -> void $ Auth.expireServerSession sessionId
  let expireCookie = Auth.mkCookieSessionExpired env (Domains.cookieDomainMaybe env)
  pure $ Servant.addHeader expireCookie $ Servant.addHeader "/" (redirectTemplate "/")
