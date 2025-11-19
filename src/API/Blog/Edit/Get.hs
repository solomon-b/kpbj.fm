{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Edit.Get where

--------------------------------------------------------------------------------

import API.Blog.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Blog.Edit.Get.Templates.Form (template)
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
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
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
    "GET /blog/:id/:slug/edit"
    ( "blog"
        :> Servant.Capture "id" BlogPosts.Id
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
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer blogPostId urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to blog edit" ()
      html <- renderTemplate hxRequest Nothing notLoggedInTemplate
      pure $ Servant.noHeader html
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
        tags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
        MaybeT $ pure $ Just (blogPost, tags)

      case mResult of
        Left err -> do
          Log.logAttention "getBlogPostById execution error" (show err)
          html <- renderTemplate hxRequest (Just userMetadata) notFoundTemplate
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo "No blog post found with id" blogPostId
          html <- renderTemplate hxRequest (Just userMetadata) notFoundTemplate
          pure $ Servant.noHeader html
        Right (Just (blogPost, tags)) -> do
          let canonicalSlug = blogPost.bpmSlug
              postIdText = display blogPostId
              slugText = display canonicalSlug
              canonicalUrl = [i|/blog/#{postIdText}/#{slugText}/edit|]

          if matchSlug canonicalSlug (Just urlSlug)
            then
              if blogPost.bpmAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then do
                  Log.logInfo "Authorized user accessing blog edit form" blogPost.bpmId
                  let editTemplate = template blogPost tags userMetadata
                  html <- renderTemplate hxRequest (Just userMetadata) editTemplate
                  pure $ Servant.noHeader html
                else do
                  Log.logInfo "User tried to edit blog post they don't own" blogPost.bpmId
                  html <- renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
                  pure $ Servant.noHeader html
            else do
              Log.logInfo "Redirecting to canonical blog edit URL" canonicalUrl
              html <- renderTemplate hxRequest (Just userMetadata) (redirectTemplate canonicalUrl)
              pure $ Servant.addHeader canonicalUrl html
