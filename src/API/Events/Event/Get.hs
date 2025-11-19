{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Event.Get where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Component.Redirect (redirectTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
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

-- | Route for event with ID and slug (canonical URL)
type RouteWithSlug =
  Observability.WithSpan
    "GET /events/:id/:slug"
    ( "events"
        :> Servant.Capture "id" Events.Id
        :> Servant.Capture "slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

-- | Route for event with ID only (redirects to canonical)
type RouteWithoutSlug =
  Observability.WithSpan
    "GET /events/:id"
    ( "events"
        :> Servant.Capture "id" Events.Id
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Handler for event with both ID and slug
handlerWithSlug ::
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
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug tracer eventId slug = handler tracer eventId (Just slug)

-- | Handler for event with ID only (always redirects)
handlerWithoutSlug ::
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
  Events.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug tracer eventId = handler tracer eventId Nothing

-- | Shared handler for both routes
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
  Events.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer eventId mUrlSlug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuerySpan (Events.getEventById eventId) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch event from database" eventId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Event not found" eventId
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right (Just event) -> do
      let canonicalSlug = Events.emSlug event
          eventIdText = display eventId
          slugText = display canonicalSlug
          canonicalUrl = [i|/events/#{eventIdText}/#{slugText}|]

      if matchSlug canonicalSlug mUrlSlug
        then renderEvent hxRequest mUserInfo event
        else renderRedirect hxRequest mUserInfo canonicalUrl

renderEvent ::
  ( Has Tracer env,
    MonadReader env m,
    Log.MonadLog m,
    MonadDB m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Events.Model ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderEvent hxRequest mUserInfo event =
  execQuerySpan (UserMetadata.getUserMetadata event.emAuthorId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch event author" (Aeson.object ["id" .= event.emAuthorId, "error" .= show err])
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (Events.emSlug event))
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logAttention "Event author not found" (Aeson.object ["id" .= event.emAuthorId])
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (Events.emSlug event))
      pure $ Servant.noHeader html
    Right (Just author) ->
      fetchTags hxRequest mUserInfo event author

renderRedirect ::
  (MonadReader env m, MonadUnliftIO m, MonadCatch m, Log.MonadLog m) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo url = do
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate url)
  pure $ Servant.addHeader url html

fetchTags ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Events.Model ->
  UserMetadata.Model ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
fetchTags hxRequest mUserInfo event author =
  execQuerySpan (Events.getEventTags event.emId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch event tags" (Aeson.object ["event" .= event.emId, "error" .= show err])
      let eventTemplate = template event [] author
      html <- renderTemplate hxRequest mUserInfo eventTemplate
      pure $ Servant.noHeader html
    Right eventTagModels -> do
      let eventTemplate = template event eventTagModels author
      html <- renderTemplate hxRequest mUserInfo eventTemplate
      pure $ Servant.noHeader html
