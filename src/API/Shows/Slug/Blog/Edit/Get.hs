{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Edit.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Edit.Get.Templates.Page (editBlogPostForm, errorTemplate, notLoggedInTemplate, permissionDeniedTemplate)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
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
    "GET /shows/:show_slug/blog/:post_slug/edit"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "blog"
        :> Servant.Capture "post_slug" Slug
        :> "edit"
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug postSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to blog post edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- Fetch blog post, show, tags, and verify host permissions in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        post <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostBySlug (display showSlug) (display postSlug))
        showModel <- MaybeT $ HT.statement () (Shows.getShowById post.showId)
        tags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost post.id)
        isHost <- lift $ HT.statement () (Shows.isUserHostOfShow (User.mId user) post.showId)
        MaybeT $ pure $ Just (post, showModel, tags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "Failed to load blog post edit form" (show err)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Failed to load blog post. Please try again."
        Right Nothing -> do
          Log.logInfo "Blog post not found" (showSlug, postSlug)
          renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Blog post not found."
        Right (Just (post, showModel, tags, isHost)) ->
          if isHost || User.mId user == post.authorId
            then do
              Log.logInfo "Authorized user accessing blog post edit form" post.id
              renderTemplate hxRequest (Just userMetadata) $ editBlogPostForm showModel post tags
            else do
              Log.logInfo "User not authorized to edit this blog post" (User.mId user, post.id)
              renderTemplate hxRequest (Just userMetadata) permissionDeniedTemplate
