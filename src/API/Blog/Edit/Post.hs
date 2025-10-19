{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Blog.Edit.Post where

--------------------------------------------------------------------------------

import API.Blog.Edit.Post.Templates.Error (errorTemplate, forbiddenTemplate, notFoundTemplate, unauthorizedTemplate)
import API.Blog.Edit.Post.Templates.Success (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
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
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /blog/:slug/edit"
    ( "blog"
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] BlogEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for blog post editing
data BlogEditForm = BlogEditForm
  { befTitle :: Text,
    befContent :: Text,
    befExcerpt :: Maybe Text,
    befStatus :: Text,
    befTags :: [Text]
  }
  deriving (Show)

instance FromForm BlogEditForm where
  fromForm :: Form.Form -> Either Text BlogEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    content <- Form.parseUnique "content" form
    excerpt <- Form.parseMaybe "excerpt" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form

    pure
      BlogEditForm
        { befTitle = title,
          befContent = content,
          befExcerpt = emptyToNothing excerpt,
          befStatus = status,
          befTags = parseTags $ fromMaybe "" tags
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map Text.strip $
      Text.splitOn "," tagText

-- | Parse blog post status from text
parseStatus :: Text -> Maybe BlogPostStatus
parseStatus "published" = Just Published
parseStatus "draft" = Just Draft
parseStatus "archived" = Just Archived
parseStatus _ = Nothing

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
  BlogEditForm ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" slug
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostBySlug slug)
        oldTags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
        MaybeT $ pure $ Just (blogPost, oldTags)

      case mResult of
        Left err -> do
          Log.logAttention "getBlogPostBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No blog post with slug: '" <> display slug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (blogPost, oldTags)) ->
          if blogPost.bpmAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then updateBlogPost hxRequest userMetadata blogPost oldTags editForm
            else do
              Log.logInfo "User attempted to edit blog post they don't own" blogPost.bpmId
              renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

updateBlogPost ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  BlogPosts.Model ->
  [BlogTags.Model] ->
  BlogEditForm ->
  m (Lucid.Html ())
updateBlogPost hxRequest userMetadata blogPost oldTags editForm = do
  case parseStatus (befStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in blog edit form" (befStatus editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid blog post status value.")
    Just parsedStatus -> do
      let newSlug = Slug.mkSlug (befTitle editForm)
          updateData =
            BlogPosts.Insert
              { BlogPosts.bpiTitle = befTitle editForm,
                BlogPosts.bpiSlug = newSlug,
                BlogPosts.bpiContent = befContent editForm,
                BlogPosts.bpiExcerpt = befExcerpt editForm,
                BlogPosts.bpiAuthorId = blogPost.bpmAuthorId,
                BlogPosts.bpiStatus = parsedStatus
              }

      mUpdateResult <- execTransactionSpan $ runMaybeT $ do
        _ <- MaybeT $ HT.statement () (BlogPosts.updateBlogPost blogPost.bpmId updateData)
        lift $ traverse_ (\tag -> HT.statement () (BlogPosts.removeTagFromPost blogPost.bpmId tag.btmId)) oldTags
        lift $ updatePostTags blogPost.bpmId editForm
        MaybeT $ pure $ Just ()

      case mUpdateResult of
        Left err -> do
          Log.logInfo "Failed to update blog post" (blogPost.bpmId, show err)
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
        Right Nothing -> do
          Log.logInfo "Blog post update returned Nothing" blogPost.bpmId
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update blog post. Please try again.")
        Right (Just _) -> do
          Log.logInfo "Successfully updated blog post" blogPost.bpmId
          renderTemplate hxRequest (Just userMetadata) (template newSlug)

-- | Update tags for a blog post (add new ones)
updatePostTags ::
  BlogPosts.Id ->
  BlogEditForm ->
  HT.Transaction ()
updatePostTags postId form = do
  let newTags = befTags form
  unless (null newTags) $
    traverse_ (createOrAssociateTag postId) newTags

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  BlogPosts.Id ->
  Text ->
  HT.Transaction ()
createOrAssociateTag postId tagName = do
  mExistingTag <- HT.statement () (BlogTags.getTagByName tagName)
  case mExistingTag of
    Just existingTag -> do
      -- If tag exists, associate it
      void $ HT.statement () (BlogPosts.addTagToPost postId (BlogTags.btmId existingTag))
    Nothing -> do
      -- Otherwise, create new tag and associate it
      newTagId <- HT.statement () (BlogTags.insertTag (BlogTags.Insert tagName))
      void $ HT.statement () (BlogPosts.addTagToPost postId newTagId)
