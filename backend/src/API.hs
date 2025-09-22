module API where

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Blog.Get qualified as Blog.Get
import API.Blog.New.Get qualified as Blog.New.Get
import API.Blog.New.Post qualified as Blog.New.Post
import API.Blog.Post.Get qualified as Blog.Post.Get
import API.Donate.Get qualified as Donate.Get
import API.Get qualified as Root.Get
import API.PrivacyPolicy.Get qualified as PrivacyPolicy.Get
import API.Static.Get qualified as Static.Get
import API.TermsOfService.Get qualified as TermsOfService.Get
import API.User.Login.Get qualified as User.Login.Get
import API.User.Login.Post qualified as User.Login.Post
import API.User.Logout.Get qualified as User.Logout.Get
import API.User.Logout.Post qualified as User.Logout.Post
import API.User.Register.Get qualified as User.Register.Get
import API.User.Register.Post qualified as User.Register.Post
import App qualified
import App.Config (Environment)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Hasql.Pool qualified as HSQL.Pool
import Log (MonadLog)
import OpenTelemetry.Trace (Tracer)
import Servant ((:<|>) (..))
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

runApi :: IO ()
runApi = App.runApp @API server ()

--------------------------------------------------------------------------------

type API =
  Root.Get.Route
    :<|> Static.Get.Route
    :<|> About.Get.Route
    :<|> Blog.Get.Route
    :<|> Blog.New.Get.Route
    :<|> Blog.New.Post.Route
    :<|> Blog.Post.Get.Route
    :<|> Donate.Get.Route
    :<|> PrivacyPolicy.Get.Route
    :<|> TermsOfService.Get.Route
    :<|> User.Login.Get.Route
    :<|> User.Login.Post.Route
    :<|> User.Logout.Get.Route
    :<|> User.Logout.Post.Route
    :<|> User.Register.Get.Route
    :<|> User.Register.Post.Route

--------------------------------------------------------------------------------

server ::
  ( Has Tracer env,
    MonadCatch m,
    MonadLog m,
    MonadUnliftIO m,
    MonadClock m,
    MonadDB m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Environment env
  ) =>
  Environment ->
  Servant.ServerT API m
server env =
  Root.Get.handler
    :<|> Static.Get.handler env
    :<|> About.Get.handler
    :<|> Blog.Get.handler
    :<|> Blog.New.Get.handler
    :<|> Blog.New.Post.handler
    :<|> Blog.Post.Get.handler
    :<|> Donate.Get.handler
    :<|> PrivacyPolicy.Get.handler
    :<|> TermsOfService.Get.handler
    :<|> User.Login.Get.handler
    :<|> User.Login.Post.handler
    :<|> User.Logout.Get.handler
    :<|> User.Logout.Post.handler
    :<|> User.Register.Get.handler
    :<|> User.Register.Post.handler

--------------------------------------------------------------------------------

-- | Route: GET /
rootGetLink :: Links.Link
rootGetLink = Links.safeLink (Proxy @API) (Proxy @Root.Get.Route)

-- | Route: GET /static
staticGetLink :: Links.Link
staticGetLink = Links.safeLink (Proxy @API) (Proxy @Static.Get.Route)

-- | Route: GET /about
aboutGetLink :: Links.Link
aboutGetLink = Links.safeLink (Proxy @API) (Proxy @About.Get.Route)

-- | Route: GET /blog
blogGetLink :: Maybe Int64 -> Maybe Text -> Maybe Text -> Links.Link
blogGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Get.Route)

-- | Route: GET /blog/new
blogNewGetLink :: Links.Link
blogNewGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Get.Route)

-- | Route: POST /blog/new
blogNewPostLink :: Links.Link
blogNewPostLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Post.Route)

-- | Route: GET /blog/:slug
blogPostGetLink :: Text -> Links.Link
blogPostGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Post.Get.Route)

-- | Route: GET /donate
donateGetLink :: Links.Link
donateGetLink = Links.safeLink (Proxy @API) (Proxy @Donate.Get.Route)

-- | Route: GET /user/login
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userLoginGetLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Get.Route)

-- | Route: POST /user/login
userLoginPostLink :: Maybe Text -> Links.Link
userLoginPostLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Post.Route)

-- | Route: GET /user/logout
userLogoutGetLink :: Links.Link
userLogoutGetLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Get.Route)

-- | Route: POST /user/logout
userLogoutPostLink :: Links.Link
userLogoutPostLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Post.Route)

-- | Route: GET /user/register
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> Links.Link
userRegisterGetLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Get.Route)

-- | Route: POST /user/register
userRegisterPostLink :: Links.Link
userRegisterPostLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Post.Route)

-- | Route: GET /privacy-policy
privacyPolicyGetLink :: Links.Link
privacyPolicyGetLink = Links.safeLink (Proxy @API) (Proxy @PrivacyPolicy.Get.Route)

-- | Route: GET /terms-of-service
termsOfServiceGetLink :: Links.Link
termsOfServiceGetLink = Links.safeLink (Proxy @API) (Proxy @TermsOfService.Get.Route)
