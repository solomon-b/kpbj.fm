{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Blog.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogEditGetLink, showBlogPostGetLink, showGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
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
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- URL helpers
showBlogPostGetUrl :: Slug -> Slug -> Links.URI
showBlogPostGetUrl showSlug postSlug = Links.linkURI $ showBlogPostGetLink showSlug postSlug

showBlogEditGetUrl :: Slug -> Slug -> Links.URI
showBlogEditGetUrl showSlug postSlug = Links.linkURI $ showBlogEditGetLink showSlug postSlug

showGetUrl :: Slug -> Links.URI
showGetUrl showSlug = Links.linkURI $ showGetLink showSlug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/blog/:post_slug/edit"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "blog"
        :> Servant.Capture "post_slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] ShowBlogEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Success template after blog update
successTemplate :: Slug -> Slug -> Lucid.Html ()
successTemplate showSlug newPostSlug = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Blog Post Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your blog post has been updated."

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogPostGetUrl showSlug newPostSlug}|],
          hxGet_ [i|/#{showBlogPostGetUrl showSlug newPostSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "VIEW POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
          hxGet_ [i|/#{showGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "BACK TO SHOW"

-- | Error template
errorTemplate :: Slug -> Slug -> Text -> Lucid.Html ()
errorTemplate showSlug postSlug errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "❌ Error Updating Blog Post"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogEditGetUrl showSlug postSlug}|],
          hxGet_ [i|/#{showBlogEditGetUrl showSlug postSlug}|],
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

-- | Form data for show blog post editing
data ShowBlogEditForm = ShowBlogEditForm
  { sbefTitle :: Text,
    sbefContent :: Text,
    sbefExcerpt :: Maybe Text,
    sbefStatus :: Text,
    sbefTags :: [Text]
  }
  deriving (Show)

instance FromForm ShowBlogEditForm where
  fromForm :: Form.Form -> Either Text ShowBlogEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    content <- Form.parseUnique "content" form
    excerpt <- Form.parseMaybe "excerpt" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form

    pure
      ShowBlogEditForm
        { sbefTitle = title,
          sbefContent = content,
          sbefExcerpt = emptyToNothing excerpt,
          sbefStatus = status,
          sbefTags = parseTags $ fromMaybe "" tags
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  ShowBlogEditForm ->
  m (Lucid.Html ())
handler _tracer showSlug postSlug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized blog edit attempt" (showSlug, postSlug)
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (ShowBlogPosts.getShowBlogPostBySlug (display showSlug) (display postSlug))
        showModel <- MaybeT $ HT.statement () (Shows.getShowById blogPost.showId)
        oldTags <- lift $ HT.statement () (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)
        isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) blogPost.showId)
        MaybeT $ pure $ Just (blogPost, showModel, oldTags, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getShowBlogPostBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo "No blog post found" (showSlug, postSlug)
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
      renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.slug blogPost) "Invalid blog post status value.")
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
        lift $ traverse_ (\tag -> HT.statement () (ShowBlogPosts.removeTagFromShowBlogPost blogPost.id (ShowBlogTags.sbtmId tag))) oldTags
        lift $ updatePostTags blogPost.id editForm
        MaybeT $ pure $ Just ()

      case mUpdateResult of
        Left err -> do
          Log.logInfo "Failed to update show blog post" (blogPost.id, show err)
          renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.slug blogPost) "Database error occurred. Please try again.")
        Right Nothing -> do
          Log.logInfo "Blog post update returned Nothing" blogPost.id
          renderTemplate hxRequest (Just userMetadata) (errorTemplate (Shows.slug showModel) (ShowBlogPosts.slug blogPost) "Failed to update blog post. Please try again.")
        Right (Just _) -> do
          Log.logInfo "Successfully updated show blog post" blogPost.id
          renderTemplate hxRequest (Just userMetadata) (successTemplate (Shows.slug showModel) newSlug)

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
