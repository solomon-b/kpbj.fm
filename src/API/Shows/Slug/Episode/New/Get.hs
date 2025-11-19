{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.New.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.New.Get.Templates.Error (notLoggedInTemplate, showLoadErrorTemplate)
import API.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (guard, unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
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
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:show_slug/episodes/new"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> "new"
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
handler _tracer showSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode upload" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- Fetch show, verify host permissions, and get upcoming unscheduled dates in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
        -- Admins can create episodes for any show, hosts need explicit assignment
        unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
          isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
          guard isHost
        upcomingDates <- lift $ HT.statement () (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id 4)
        MaybeT $ pure $ Just (showModel, upcomingDates)

      case mResult of
        Left err -> do
          Log.logAttention "Failed to load episode upload form" (show err)
          renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
        Right Nothing -> do
          Log.logInfo "Show not found or user not authorized" (showSlug, User.mId user)
          renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
        Right (Just (showModel, upcomingDates)) -> do
          Log.logInfo "Authorized user accessing episode upload form" showModel.id
          renderTemplate hxRequest (Just userMetadata) $ episodeUploadForm showModel upcomingDates
