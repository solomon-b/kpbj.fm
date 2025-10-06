module API.Events.Event.Get where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Templates.Page (notFoundTemplate, template)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Cont (ContT (..), evalContT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
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
    "GET /events/:slug"
    ( "events"
        :> Servant.Capture "slug" Text
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

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
  Text ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer slug cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  evalContT $ do
    mUserInfo <- ContT $ getUserInfo cookie
    event <- ContT $ getEvent isHtmxRequest mUserInfo slug
    author <- ContT $ getAuthor isHtmxRequest mUserInfo slug event
    lift $ fetchTags isHtmxRequest mUserInfo event author

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

getUserInfo ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  (Maybe UserMetadata.Model -> m a) ->
  m a
getUserInfo cookie k =
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      k Nothing
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right (Just userMetadata) ->
          k $ Just userMetadata
        _ ->
          k Nothing

getEvent ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  Maybe UserMetadata.Model ->
  Text ->
  (Events.EventModel -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
getEvent isHtmxRequest mUserInfo slug k =
  execQuerySpan (Events.getEventBySlug slug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch event from database" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Event not found" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just event) ->
      k event

getAuthor ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  -- | Logged in user:
  Maybe UserMetadata.Model ->
  Text ->
  Events.EventModel ->
  -- | Author of the event:
  (UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
getAuthor isHtmxRequest mUserInfo slug event k =
  execQuerySpan (UserMetadata.getUserMetadata event.emAuthorId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch event author" (Aeson.object ["id" .= event.emAuthorId, "error" .= show err])
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logAttention "Event author not found" (Aeson.object ["id" .= event.emAuthorId])
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just author) ->
      k author

fetchTags ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  Maybe UserMetadata.Model ->
  Events.EventModel ->
  UserMetadata.Model ->
  m (Lucid.Html ())
fetchTags isHtmxRequest mUserInfo event author =
  execQuerySpan (Events.getEventTags event.emId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch event tags" (Aeson.object ["event" .= event.emId, "error" .= show err])
      let eventTemplate = template event [] (UserMetadata.toDomain author)
      renderTemplate isHtmxRequest mUserInfo eventTemplate
    Right eventTagModels -> do
      let eventTags = map Events.toDomainEventTag eventTagModels
      let eventTemplate = template event eventTags (UserMetadata.toDomain author)
      renderTemplate isHtmxRequest mUserInfo eventTemplate
