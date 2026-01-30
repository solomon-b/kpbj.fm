module API.User.Logout.Get.Handler where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
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
        '[ Servant.Header "HX-Redirect" Text
         ]
        (Lucid.Html ())
    )
handler Auth.Authz {..} = do
  Auth.expireServerSession (Session.dSessionId $ Session.toDomain authzSession) >>= \case
    Left err -> do
      throwErr $ InternalServerError $ Text.pack $ show err
    Right _ ->
      pure $ Servant.addHeader "/" (redirectTemplate "/")
