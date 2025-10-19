{-# LANGUAGE ViewPatterns #-}

module API.Events.Event.Get where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
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

type Route =
  Observability.WithSpan
    "GET /events/:slug"
    ( "events"
        :> Servant.Capture "slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuerySpan (Events.getEventBySlug slug) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch event from database" (show err)
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Event not found" slug
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just event) ->
      execQuerySpan (UserMetadata.getUserMetadata event.emAuthorId) >>= \case
        Left err -> do
          Log.logAttention "Failed to fetch event author" (Aeson.object ["id" .= event.emAuthorId, "error" .= show err])
          renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
        Right Nothing -> do
          Log.logAttention "Event author not found" (Aeson.object ["id" .= event.emAuthorId])
          renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
        Right (Just author) ->
          fetchTags hxRequest mUserInfo event author

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
  m (Lucid.Html ())
fetchTags hxRequest mUserInfo event author =
  execQuerySpan (Events.getEventTags event.emId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch event tags" (Aeson.object ["event" .= event.emId, "error" .= show err])
      let eventTemplate = template event [] author
      renderTemplate hxRequest mUserInfo eventTemplate
    Right eventTagModels -> do
      let eventTemplate = template event eventTagModels author
      renderTemplate hxRequest mUserInfo eventTemplate
