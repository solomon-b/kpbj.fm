{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Event.Get.Handler where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Templates.Page (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContentM)
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for event with both ID and slug
handlerWithSlug ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug eventId slug = handler eventId (Just slug)

-- | Handler for event with ID only (always redirects)
handlerWithoutSlug ::
  Events.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug eventId = handler eventId Nothing

-- | Shared handler for both routes
handler ::
  Events.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler eventId mUrlSlug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  execQuery (Events.getEventById eventId) >>= \case
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
  HxRequest ->
  Maybe UserMetadata.Model ->
  Events.Model ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderEvent hxRequest mUserInfo event = do
  storageBackend <- asks getter
  execQuery (UserMetadata.getUserMetadata event.emAuthorId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch event author" (Aeson.object ["id" .= event.emAuthorId, "error" .= show err])
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (Events.emSlug event))
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logAttention "Event author not found" (Aeson.object ["id" .= event.emAuthorId])
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate (Events.emSlug event))
      pure $ Servant.noHeader html
    Right (Just author) -> do
      renderedDescription <- renderContentM (Events.emDescription event)
      let eventTemplate = template storageBackend event author renderedDescription
      html <- renderTemplate hxRequest mUserInfo eventTemplate
      pure $ Servant.noHeader html

renderRedirect ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo url = do
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate url)
  pure $ Servant.addHeader url html
