module API.Get where

--------------------------------------------------------------------------------

import API.Get.Templates (template)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /"
    ( Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer cookie hxRequest = do
  loginState <- Auth.userLoginState cookie
  let isHtmxRequest = case hxRequest of
        Just "true" -> True
        _ -> False

  case loginState of
    Auth.IsNotLoggedIn ->
      if isHtmxRequest
        then loadContentOnly template
        else loadFrame template
    Auth.IsLoggedIn user -> do
      eUserMetadata <- execQuerySpan (UserMetadata.getUserMetadata (User.mId user))
      case eUserMetadata of
        Left _err ->
          -- Database error
          if isHtmxRequest
            then loadContentOnly template
            else loadFrame template
        Right Nothing ->
          -- No metadata found
          if isHtmxRequest
            then loadContentOnly template
            else loadFrame template
        Right (Just userMetadata) ->
          if isHtmxRequest
            then loadContentOnly template
            else loadFrameWithUser userMetadata template
