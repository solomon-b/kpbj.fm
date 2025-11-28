{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Edit.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink, hostDashboardGetLink, userLoginGetLink)
import API.Events.Edit.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectTemplate, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /events/:id/:slug/edit"
    ( "events"
        :> Servant.Capture "id" Events.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
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
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer eventId urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to event edit" ()
      let banner = BannerParams Error "Access Denied" "You must be logged in to edit events."
      html <- renderTemplate hxRequest Nothing (redirectWithBanner [i|/#{userLoginGetUrl}|] banner)
      pure $ Servant.noHeader html
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventById eventId)
        tags <- lift $ HT.statement () (Events.getEventTags event.emId)
        MaybeT $ pure $ Just (event, tags)

      case mResult of
        Left err -> do
          Log.logAttention "getEventById execution error" (show err)
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{eventsGetUrl}|] banner)
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo "No event found with id" eventId
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{eventsGetUrl}|] banner)
          pure $ Servant.noHeader html
        Right (Just (event, tags)) -> do
          let canonicalSlug = event.emSlug
              eventIdText = display eventId
              slugText = display canonicalSlug
              canonicalUrl = [i|/events/#{eventIdText}/#{slugText}/edit|]

          if matchSlug canonicalSlug (Just urlSlug)
            then
              if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then do
                  Log.logInfo "Authorized user accessing event edit form" event.emId
                  let editTemplate = template event tags userMetadata
                  html <- renderTemplate hxRequest (Just userMetadata) editTemplate
                  pure $ Servant.noHeader html
                else do
                  Log.logInfo "User tried to edit event they don't own" event.emId
                  let banner = BannerParams Error "Access Denied" "You can only edit events you created or have staff permissions."
                  html <- renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{hostDashboardGetUrl}|] banner)
                  pure $ Servant.noHeader html
            else do
              Log.logInfo "Redirecting to canonical event edit URL" canonicalUrl
              html <- renderTemplate hxRequest (Just userMetadata) (redirectTemplate canonicalUrl)
              pure $ Servant.addHeader canonicalUrl html
