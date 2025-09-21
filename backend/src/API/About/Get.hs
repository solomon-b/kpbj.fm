module API.About.Get where

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
  "about"
    :> Servant.Header "Cookie" Text
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

template :: Lucid.Html ()
template = do
  Lucid.div_ [Lucid.class_ "prose prose-lg mx-auto text-gray-800"] $ do
    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] $
      "KPBJ is an independent, community-driven radio station based in Shadow Hills, California."

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] $
      "KPBJ is a platform to transmit what echoes in the reach of our airwaves. We share diverse voices and artists from the San Fernando Valley, Los Angeles, and beyond through a blend of music, conversation, and storytelling."

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] $
      "Our online stream will be operational in the Fall of 2025, and our FM broadcast on 95.9 FM will follow soon after. Currently, we are run only on volunteer time and the support of individual donors."

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed font-medium"] $
      "We believe that The Valley is the center of the universe and we want to share it with you."

    Lucid.p_ [Lucid.class_ "mb-8 leading-relaxed"] $
      "KPBJ-FM is operated by Sun Valley Arts and Culture, a 501(c)3 nonprofit arts organization."

    Lucid.p_ [Lucid.class_ "text-center font-medium"] $
      "Want to get involved? Sign up for our newsletter to learn more."

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
  Observability.handlerSpan "GET /about" $ do
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
