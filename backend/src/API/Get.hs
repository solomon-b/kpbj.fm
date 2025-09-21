module API.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
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
    Has HSQL.Pool.Pool env
  ) =>
  Maybe Text ->
  m (Lucid.Html ())
handler cookie =
  Observability.handlerSpan "GET /" $ do
    _loginState <- Auth.userLoginState cookie
    loadFrame template
