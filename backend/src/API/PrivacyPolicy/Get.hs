module API.PrivacyPolicy.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrame, loadFrameWithUser)
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
    "GET /privacy-policy"
    ( "privacy-policy"
        :> Servant.Header "Cookie" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

template :: Lucid.Html ()
template = do
  Lucid.div_ [Lucid.class_ "prose prose-lg text-gray-800 max-w-4xl"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-8"] "Privacy Policy"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-6"] "Last updated: January 2025"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "KPBJ-FM is committed to protecting your privacy. This Privacy Policy explains how we collect, use, and safeguard your information when you use our website and services."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Information We Collect"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "Information You Provide"
    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-4 space-y-2"] $ do
      Lucid.li_ "Account information (name, email address, username)"
      Lucid.li_ "Profile information for hosts and volunteers"
      Lucid.li_ "Comments and content you submit"
      Lucid.li_ "Communication with our staff"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "Automatically Collected Information"
    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-4 space-y-2"] $ do
      Lucid.li_ "Log data (IP address, browser type, pages visited)"
      Lucid.li_ "Cookies and similar tracking technologies"
      Lucid.li_ "Usage patterns and preferences"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "How We Use Your Information"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Provide and maintain our radio streaming services"
      Lucid.li_ "Manage user accounts and authentication"
      Lucid.li_ "Send newsletters and important updates"
      Lucid.li_ "Enable community features (comments, discussions)"
      Lucid.li_ "Improve our services and user experience"
      Lucid.li_ "Comply with legal obligations"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Information Sharing"

    Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] "We do not sell, trade, or otherwise transfer your personal information to third parties except as described below:"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "With your consent"
      Lucid.li_ "To comply with legal requirements"
      Lucid.li_ "To protect our rights and prevent fraud"
      Lucid.li_ "With service providers who assist in operations (hosting, analytics)"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Data Security"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "We implement appropriate security measures to protect your personal information against unauthorized access, alteration, disclosure, or destruction. However, no internet transmission is 100% secure."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Your Rights"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Access and update your account information"
      Lucid.li_ "Delete your account and associated data"
      Lucid.li_ "Opt out of non-essential communications"
      Lucid.li_ "Request information about data we collect"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Contact Us"

    Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] "If you have questions about this Privacy Policy, please contact us:"

    Lucid.ul_ [Lucid.class_ "list-none mb-6 space-y-1"] $ do
      Lucid.li_ "Email: contact@kpbj.fm"
      Lucid.li_ "Sun Valley Arts and Culture"
      Lucid.li_ "Shadow Hills, California"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mt-8"] "This Privacy Policy may be updated from time to time. We will notify users of significant changes."

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
  m (Lucid.Html ())
handler _tracer cookie = do
  loginState <- Auth.userLoginState cookie
  case loginState of
    Auth.IsNotLoggedIn ->
      loadFrame template
    Auth.IsLoggedIn user -> do
      eUserMetadata <- execQuerySpan (UserMetadata.getUserMetadata (User.mId user))
      case eUserMetadata of
        Left _err ->
          loadFrame template
        Right Nothing ->
          loadFrame template
        Right (Just userMetadata) ->
          loadFrameWithUser userMetadata template
