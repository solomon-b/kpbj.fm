module API.Get where

--------------------------------------------------------------------------------

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
import Lucid.Base (makeAttributes)
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

template :: Lucid.Html ()
template = do
  Lucid.a_ [Lucid.href_ "https://fccdata.org/?call=kpbj&facid=&city=&state=&ccode=1&country=US"] $ do
    Lucid.img_ [Lucid.src_ "/static/range.png"]

  Lucid.section_ [Lucid.class_ "mt-12 text-center"] $ do
    Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-4"] "Stay in the Loop"

    Lucid.iframe_ [Lucid.name_ "hidden_iframe", Lucid.id_ "hidden_iframe", Lucid.style_ "display:none;"] ""

    Lucid.form_ [Lucid.action_ "https://docs.google.com/forms/u/0/d/e/1FAIpQLSfeM91iQ_A7ybaa070b8jiznHNRIJ_2JU0F7wJjo7vAvkS3tQ/formResponse", Lucid.method_ "POST", Lucid.target_ "hidden_iframe", Lucid.onsubmit_ "handleSubmit(event)", Lucid.class_ "w-full max-w-sm"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center border-b border-gray-500 py-2"] $ do
        Lucid.input_ [Lucid.type_ "email", Lucid.name_ "entry.936311333", Lucid.required_ "", Lucid.placeholder_ "you@example.com", makeAttributes "aria-label" "Email Address", Lucid.class_ "appearance-none bg-transparent border-none w-full text-gray-700 mr-3 py-1 px-2 leading-tight focus:outline-none"]
        Lucid.button_ [Lucid.type_ "submit", Lucid.id_ "subscribe-button", Lucid.class_ "px-3 py-2 text-xs font-medium text-center inline-flex items-center text-white bg-gray-700 rounded-md hover:bg-gray-800 focus:ring-4 focus:outline-none focus:ring-gray-300 dark:bg-gray-600 dark:hover:bg-gray-700 dark:focus:ring-gray-800"] $ do
          "Subscribe"
      Lucid.p_ [Lucid.id_ "form-confirmation", Lucid.class_ "mt-2 text-green-700", Lucid.style_ "display: none;"] "Thanks for subscribing!"
  Lucid.script_ "function handleSubmit(event) { const form = event.target; const confirmation = form.querySelector('#form-confirmation'); const button = form.querySelector('#subscribe-button'); button.disabled = true; button.innerText = 'Submitting...'; setTimeout(() => { confirmation.style.display = 'block'; form.reset(); button.disabled = false; button.innerText = 'Subscribe'; }, 500); }"

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
