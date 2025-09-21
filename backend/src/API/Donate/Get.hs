module API.Donate.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (UserInfo (..), loadFrame, loadFrameWithUser)
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
  "donate"
    :> Servant.Header "Cookie" Text
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

template :: Lucid.Html ()
template = do
  Lucid.div_ [Lucid.id_ "paypal-container-YBRDJJA6AGBL6", Lucid.class_ "w-96"] ""
  Lucid.script_ [Lucid.src_ "https://www.paypal.com/sdk/js?client-id=BAA56A9gcLxsoKcdDF1ipwRFhXfp8nondkp4mIJClY5tiW5pFR9C1kMlHNKYFrRYTeecQ-MzTmxkjAkFFQ&components=hosted-buttons&enable-funding=venmo&currency=USD"] (mempty :: Text)
  Lucid.script_ [] ("paypal.HostedButtons({ hostedButtonId: \"YBRDJJA6AGBL6\" }).render(\"#paypal-container-YBRDJJA6AGBL6\")" :: Text)

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
  Maybe Text ->
  m (Lucid.Html ())
handler cookie =
  Observability.handlerSpan "GET /donate" $ do
    loginState <- Auth.userLoginState cookie
    case loginState of
      Auth.IsNotLoggedIn ->
        loadFrame template
      Auth.IsLoggedIn user -> do
        eUserMetadata <- execQuerySpan (UserMetadata.getUserMetadata (User.mId user))
        case eUserMetadata of
          Left _err ->
            -- Database error
            loadFrame template
          Right Nothing ->
            -- No metadata found
            loadFrame template
          Right (Just userMetadata) ->
            let userInfo = UserInfo {userDisplayName = UserMetadata.mDisplayName userMetadata}
             in loadFrameWithUser userInfo template
