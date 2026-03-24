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
import Component.Flash (throwHxRedirect)
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
  AppM (Lucid.Html ())
handlerWithSlug eventId slug = handler eventId (Just slug)

-- | Handler for event with ID only (always redirects)
handlerWithoutSlug ::
  Events.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handlerWithoutSlug eventId = handler eventId Nothing

-- | Shared handler for both routes
handler ::
  Events.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler eventId mUrlSlug cookie (foldHxReq -> hxRequest) =
  runExceptT innerAction >>= \case
    Right result -> pure result
    Left err -> do
      logHandlerError "Event" err
      case err of
        NotFound resource -> renderInline (notFoundContent resource)
        NotAuthenticated ->
          let (url, flash) = errorRedirectParams apiLinks.rootGet err
           in throwHxRedirect url (Just flash)
        NotAuthorized _ _ ->
          let (url, flash) = errorRedirectParams apiLinks.rootGet err
           in throwHxRedirect url (Just flash)
        DatabaseError _ -> renderInline (errorContent "Something went wrong. Please try again.")
        UserSuspended -> renderInline (errorContent "Your account is suspended.")
        ValidationError msg -> renderInline (errorContent msg)
        HandlerFailure msg -> renderInline (errorContent msg)
  where
    innerAction = do
      mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
      vd <- action eventId mUrlSlug
      case vd of
        EventRedirect canonicalUrl ->
          lift $ throwHxRedirect canonicalUrl Nothing
        EventContent backend event -> do
          renderedDescription <- lift $ renderContentM (Events.emDescription event)
          lift $ renderTemplate hxRequest mUserInfo (template backend event renderedDescription)
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
