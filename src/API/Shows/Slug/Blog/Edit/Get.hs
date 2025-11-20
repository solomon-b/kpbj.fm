{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Edit.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Edit.Get.Templates.Page (editBlogPostForm, errorTemplate, notLoggedInTemplate, permissionDeniedTemplate)
import App.Common (getUserInfo, renderTemplate)
import Component.Redirect (redirectTemplate)
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
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
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
    "GET /shows/:show_id/blog/:post_id/:slug/edit"
    ( "shows"
        :> Servant.Capture "show_id" Shows.Id
        :> "blog"
        :> Servant.Capture "post_id" ShowBlogPosts.Id
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
  Shows.Id ->
  ShowBlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showId postId urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to blog post edit" ()
      html <- renderTemplate hxRequest Nothing notLoggedInTemplate
      pure $ Servant.noHeader html
    Just (user, userMetadata) -> do
      -- Fetch blog post, show, tags, and verify host permissions in a transaction
      mResult <- execTransactionSpan $ runMaybeT $ do
        post <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostById postId)
        showModel <- MaybeT $ HT.statement () (Shows.getShowById post.showId)
        tags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost post.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) post.showId)
        MaybeT $ pure $ Just (post, showModel, tags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "Failed to load blog post edit form" (show err)
          html <- renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Failed to load blog post. Please try again."
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo "Blog post not found" (showId, postId)
          html <- renderTemplate hxRequest (Just userMetadata) $ errorTemplate "Blog post not found."
          pure $ Servant.noHeader html
        Right (Just (post, showModel, tags, isHost)) -> do
          let canonicalSlug = post.slug
              showIdText = display showId
              postIdText = display postId
              slugText = display canonicalSlug
              canonicalUrl = [i|/shows/#{showIdText}/blog/#{postIdText}/#{slugText}/edit|]

          if matchSlug canonicalSlug (Just urlSlug)
            then
              if isHost || User.mId user == post.authorId
                then do
                  Log.logInfo "Authorized user accessing blog post edit form" post.id
                  html <- renderTemplate hxRequest (Just userMetadata) $ editBlogPostForm showModel post tags
                  pure $ Servant.noHeader html
                else do
                  Log.logInfo "User not authorized to edit this blog post" (User.mId user, post.id)
                  html <- renderTemplate hxRequest (Just userMetadata) permissionDeniedTemplate
                  pure $ Servant.noHeader html
            else do
              Log.logInfo "Redirecting to canonical show blog edit URL" canonicalUrl
              html <- renderTemplate hxRequest (Just userMetadata) (redirectTemplate canonicalUrl)
              pure $ Servant.addHeader canonicalUrl html
