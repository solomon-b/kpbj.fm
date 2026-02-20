{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Event.Get.Handler where

--------------------------------------------------------------------------------

import API.Events.Event.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError (..), errorContent, errorRedirectParams, logHandlerError, notFoundContent, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Redirect (buildRedirectUrl, redirectTemplate, redirectWithBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Markdown (renderContentM)
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

data EventViewData
  = EventRedirect Text
  | EventContent StorageBackend Events.Model

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
handler eventId mUrlSlug cookie (foldHxReq -> hxRequest) =
  runExceptT innerAction >>= \case
    Right result -> pure result
    Left err -> do
      logHandlerError "Event" err
      case err of
        NotFound resource -> Servant.noHeader <$> renderInline (notFoundContent resource)
        NotAuthenticated ->
          let (url, banner) = errorRedirectParams apiLinks.rootGet err
           in pure $ Servant.addHeader (buildRedirectUrl url banner) (redirectWithBanner url banner)
        NotAuthorized _ _ ->
          let (url, banner) = errorRedirectParams apiLinks.rootGet err
           in pure $ Servant.addHeader (buildRedirectUrl url banner) (redirectWithBanner url banner)
        DatabaseError _ -> Servant.noHeader <$> renderInline (errorContent "Something went wrong. Please try again.")
        UserSuspended -> Servant.noHeader <$> renderInline (errorContent "Your account is suspended.")
        ValidationError msg -> Servant.noHeader <$> renderInline (errorContent msg)
        HandlerFailure msg -> Servant.noHeader <$> renderInline (errorContent msg)
  where
    innerAction = do
      mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
      vd <- action eventId mUrlSlug
      case vd of
        EventRedirect canonicalUrl -> do
          html <- lift $ renderTemplate hxRequest mUserInfo (redirectTemplate canonicalUrl)
          pure $ Servant.addHeader canonicalUrl html
        EventContent backend event -> do
          renderedDescription <- lift $ renderContentM (Events.emDescription event)
          html <- lift $ renderTemplate hxRequest mUserInfo (template backend event renderedDescription)
          pure $ Servant.noHeader html
    renderInline content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch event, verify slug, render description.
action ::
  Events.Id ->
  Maybe Slug ->
  ExceptT HandlerError AppM EventViewData
action eventId mUrlSlug = do
  event <-
    fromMaybeM (throwNotFound "Event") $
      fromRightM throwDatabaseError $
        execQuery (Events.getEventById eventId)

  let canonicalSlug = Events.emSlug event
      eventIdText = display eventId
      slugText = display canonicalSlug
      canonicalUrl = [i|/events/#{eventIdText}/#{slugText}|]

  if matchSlug canonicalSlug mUrlSlug
    then do
      backend <- asks getter
      pure $ EventContent backend event
    else pure $ EventRedirect canonicalUrl
