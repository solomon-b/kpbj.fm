module API.Get where

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
import Lucid (alt_, class_, div_, img_, p_, src_)
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

template :: Lucid.Html ()
template =
  div_ [class_ "w-full px-4"] $ do
    div_ [class_ "hero-content wow fadeInUp mx-auto max-w-[780px] text-center"] $ do
      p_ [class_ "mx-auto mb-9 max-w-[600px] text-base font-medium sm:text-lg sm:leading-[1.44]"] $ do
        img_ [src_ "static/WWW-LetShare.svg.png", alt_ "logo"]
        "Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind."

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
  Observability.handlerSpan "GET /" $ do
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
            let userInfo = UserInfo { userDisplayName = UserMetadata.mDisplayName userMetadata }
             in loadFrameWithUser userInfo template
