{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Get.Templates.Page (errorTemplate, newBlogPostForm, notLoggedInTemplate)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
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
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

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
      Log.logInfo "Unauthorized access to new blog post form" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- Fetch show and verify host permissions in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
        -- Admins can create blog posts for any show, hosts need explicit assignment
        unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
          isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
          guard isHost
        MaybeT $ pure $ Just showModel

      case mResult of
        Left err -> do
          Log.logAttention "Failed to load new blog post form" (show err)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Failed to load blog post form. Please try again."
        Right Nothing -> do
          Log.logInfo "Show not found or user not authorized" (showSlug, User.mId user)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "You are not authorized to create blog posts for this show."
        Right (Just showModel) -> do
          Log.logInfo "Authorized user accessing new blog post form" showModel.id
          -- Fetch shows for dashboard sidebar
          allShows <-
            if UserMetadata.isAdmin userMetadata.mUserRole
              then either (const []) id <$> execQuerySpan Shows.getAllActiveShows
              else either (const []) id <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
          renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavBlog Nothing Nothing $ newBlogPostForm showModel
