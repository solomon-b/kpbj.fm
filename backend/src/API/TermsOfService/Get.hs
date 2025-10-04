module API.TermsOfService.Get where

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
    "GET /terms-of-service"
    ( "terms-of-service"
        :> Servant.Header "Cookie" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

template :: Lucid.Html ()
template = do
  Lucid.div_ [Lucid.class_ "prose prose-lg text-gray-800 max-w-4xl"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-8"] "Terms of Service"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-6"] "Last updated: January 2025"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "Welcome to KPBJ-FM. These Terms of Service govern your use of our website, radio streaming services, and community features. By accessing or using our services, you agree to be bound by these terms."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Acceptance of Terms"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "By creating an account or using our services, you acknowledge that you have read, understood, and agree to be bound by these Terms of Service and our Privacy Policy."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Description of Service"

    Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] "KPBJ-FM is a community radio station that provides:"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Internet radio streaming services"
      Lucid.li_ "Community discussion forums and commenting"
      Lucid.li_ "Event listings and calendar"
      Lucid.li_ "Host application and content management tools"
      Lucid.li_ "Newsletter and communication services"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "User Accounts and Responsibilities"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "Account Creation"
    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-4 space-y-2"] $ do
      Lucid.li_ "You must provide accurate and complete information"
      Lucid.li_ "You are responsible for maintaining account security"
      Lucid.li_ "One account per person"
      Lucid.li_ "You must be 13 years or older to create an account"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "Prohibited Conduct"
    Lucid.p_ [Lucid.class_ "mb-3 leading-relaxed"] "You agree not to:"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Post harmful, offensive, or illegal content"
      Lucid.li_ "Harass or bully other users"
      Lucid.li_ "Spam or send unsolicited messages"
      Lucid.li_ "Violate copyright or intellectual property rights"
      Lucid.li_ "Attempt to hack or disrupt our services"
      Lucid.li_ "Impersonate others or create fake accounts"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Content and Intellectual Property"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "User-Generated Content"
    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-4 space-y-2"] $ do
      Lucid.li_ "You retain ownership of content you create"
      Lucid.li_ "You grant us license to use, display, and distribute your content"
      Lucid.li_ "You are responsible for ensuring you have rights to content you post"
      Lucid.li_ "We reserve the right to remove content that violates these terms"

    Lucid.h3_ [Lucid.class_ "text-xl font-semibold mt-6 mb-3"] "KPBJ Content"
    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "All KPBJ-FM content, including logos, show content, and website design, is protected by copyright and other intellectual property laws."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Host Guidelines"

    Lucid.p_ [Lucid.class_ "mb-3 leading-relaxed"] "If you are accepted as a show host, you additionally agree to:"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Follow FCC broadcasting guidelines and standards"
      Lucid.li_ "Respect our community values and mission"
      Lucid.li_ "Maintain professional conduct on and off air"
      Lucid.li_ "Complete required training and orientation"
      Lucid.li_ "Honor scheduled broadcasting commitments"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Privacy and Data"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "Your privacy is important to us. Please review our Privacy Policy to understand how we collect, use, and protect your information."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Disclaimers and Limitations"

    Lucid.ul_ [Lucid.class_ "list-disc pl-6 mb-6 space-y-2"] $ do
      Lucid.li_ "Services are provided \"as is\" without warranties"
      Lucid.li_ "We are not liable for user-generated content"
      Lucid.li_ "Service availability may be interrupted for maintenance"
      Lucid.li_ "We reserve the right to modify or discontinue services"

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Termination"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "We may terminate or suspend your account for violations of these terms. You may delete your account at any time through your account settings."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Changes to Terms"

    Lucid.p_ [Lucid.class_ "mb-6 leading-relaxed"] "We may update these Terms of Service from time to time. Users will be notified of significant changes via email or website notice."

    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mt-8 mb-4"] "Contact Information"

    Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] "Questions about these Terms of Service? Contact us:"

    Lucid.ul_ [Lucid.class_ "list-none mb-6 space-y-1"] $ do
      Lucid.li_ "Email: contact@kpbj.fm"
      Lucid.li_ "Sun Valley Arts and Culture"
      Lucid.li_ "Shadow Hills, California"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mt-8"] "These Terms of Service are effective as of the date listed above and govern your use of KPBJ-FM services."

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
