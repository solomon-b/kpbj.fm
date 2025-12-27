{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Post.Route (ShowBlogEditForm (..))
import API.Links (apiLinks, dashboardBlogsLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardBlogsDetailUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogsDetailUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.detail showSlug postId

dashboardBlogsEditGetUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogsEditGetUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.editGet showSlug postId

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

--------------------------------------------------------------------------------

-- | Error template
errorTemplate :: Slug -> ShowBlogPosts.Id -> Text -> Lucid.Html ()
errorTemplate showSlug postId errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Error Updating Blog Post"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardBlogsEditGetUrl showSlug postId}|],
          hxGet_ [i|/#{dashboardBlogsEditGetUrl showSlug postId}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "TRY AGAIN"

unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Login Required"
    Lucid.p_ "You must be logged in to edit blog posts."

forbiddenTemplate :: Lucid.Html ()
forbiddenTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ "You are not authorized to edit this blog post."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Blog Post Not Found"
    Lucid.p_ "The blog post you're trying to edit does not exist."

--------------------------------------------------------------------------------

-- | Parse blog post status from text
parseStatus :: Text -> Maybe BlogPostStatus
parseStatus "published" = Just Published
parseStatus "draft" = Just Draft
parseStatus "deleted" = Just Deleted
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
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  ShowBlogEditForm ->
  m (Lucid.Html ())
handler _tracer showSlug postId cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" (showSlug, postId)
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (_user, userMetadata)
      | UserMetadata.isSuspended userMetadata -> do
          let banner = BannerParams Error "Account Suspended" "Your account has been suspended. You cannot edit blog posts."
          renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{rootGetUrl}|] banner)
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostById postId)
        showModel <- MaybeT $ HT.statement () (Shows.getShowById blogPost.showId)
        -- Verify the show slug matches
        MaybeT $ pure $ if showModel.slug == showSlug then Just () else Nothing
        oldTags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) blogPost.showId)
        MaybeT $ pure $ Just (blogPost, showModel, oldTags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getShowBlogPostById execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo "No blog post found" (showSlug, postId)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (blogPost, showModel, oldTags, isHost)) ->
          if blogPost.authorId == User.mId user || isHost
            then updateBlogPost hxRequest userMetadata showModel blogPost oldTags editForm
            else do
              Log.logInfo "User attempted to edit blog post they're not authorized for" blogPost.id
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
  Shows.Model ->
  ShowBlogPosts.Model ->
  [ShowBlogTags.Model] ->
  ShowBlogEditForm ->
  m (Lucid.Html ())
updateBlogPost hxRequest userMetadata showModel blogPost oldTags editForm = do
  case parseStatus (sbefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in blog edit form" (sbefStatus editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.id blogPost) "Invalid blog post status value.")
    Just parsedStatus -> do
      let newSlug = Slug.mkSlug (sbefTitle editForm)
          updateData =
            ShowBlogPosts.Insert
              { ShowBlogPosts.sbpiId = blogPost.showId,
                ShowBlogPosts.sbpiTitle = sbefTitle editForm,
                ShowBlogPosts.sbpiSlug = newSlug,
                ShowBlogPosts.sbpiContent = sbefContent editForm,
                ShowBlogPosts.sbpiExcerpt = sbefExcerpt editForm,
                ShowBlogPosts.sbpiAuthorId = blogPost.authorId,
                ShowBlogPosts.sbpiStatus = parsedStatus
              }

      mUpdateResult <- execTransactionSpan $ runMaybeT $ do
        _ <- MaybeT $ HT.statement () (ShowBlogPosts.updateShowBlogPost blogPost.id updateData)
        lift $ traverse_ (HT.statement () . ShowBlogPosts.removeTagFromShowBlogPost blogPost.id . ShowBlogTags.sbtmId) oldTags
        lift $ updatePostTags blogPost.id editForm
        MaybeT $ pure $ Just ()

      case mUpdateResult of
        Left err -> do
          Log.logInfo "Failed to update show blog post" (blogPost.id, show err)
          renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.id blogPost) "Database error occurred. Please try again.")
        Right Nothing -> do
          Log.logInfo "Blog post update returned Nothing" blogPost.id
          renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.id blogPost) "Failed to update blog post. Please try again.")
        Right (Just _) -> do
          Log.logInfo "Successfully updated show blog post" blogPost.id
          let postId' = blogPost.id
              detailUrl = [i|/#{dashboardBlogsDetailUrl (Shows.slug showModel) postId'}|]
              banner = BannerParams Success "Blog Post Updated" "Your blog post has been updated successfully."
          renderTemplate hxRequest (Just userMetadata) (redirectWithBanner detailUrl banner)

-- | Update tags for a blog post (add new ones)
updatePostTags ::
  ShowBlogPosts.Id ->
  ShowBlogEditForm ->
  HT.Transaction ()
updatePostTags postId editForm =
  traverse_ (createOrAssociateTag postId) (sbefTags editForm)

-- | Create a new tag or associate an existing one with a post
createOrAssociateTag ::
  ShowBlogPosts.Id ->
  Text ->
  HT.Transaction ()
createOrAssociateTag postId tagName = do
  mTag <- HT.statement () (ShowBlogTags.getShowBlogTagByName tagName)
  case mTag of
    Just existingTag -> do
      -- If tag exists, associate it
      HT.statement () (ShowBlogPosts.addTagToShowBlogPost postId (ShowBlogTags.sbtmId existingTag))
    Nothing -> do
      -- otherwise, create new tag and associate it
      newTagId <- HT.statement () (ShowBlogTags.insertShowBlogTag (ShowBlogTags.Insert tagName))
      HT.statement () (ShowBlogPosts.addTagToShowBlogPost postId newTagId)
