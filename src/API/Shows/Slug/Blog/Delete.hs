{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Blog.Delete where

--------------------------------------------------------------------------------

import App.Common (getUserInfo)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Base qualified as LucidBase
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "DELETE /shows/:show_id/:show_slug/blog/:post_id/:post_slug"
    ( "shows"
        :> Servant.Capture "show_id" Shows.Id
        :> Servant.Capture "show_slug" Slug
        :> "blog"
        :> Servant.Capture "post_id" ShowBlogPosts.Id
        :> Servant.Capture "post_slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
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
  Slug ->
  ShowBlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer showId _showSlug postId _postSlug cookie = do
  -- Fetch the show by ID
  execQuerySpan (Shows.getShowById showId) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      pure $ renderSimpleErrorBanner "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Delete failed: Show not found" (Aeson.object ["showId" .= showId])
      pure $ renderSimpleErrorBanner "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          pure $ renderSimpleErrorBanner "You must be logged in to delete blog posts."
        Just (user, userMetadata) -> do
          execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
            Left err -> do
              Log.logInfo "Delete failed: Failed to fetch blog post" (Aeson.object ["error" .= show err])
              pure $ renderSimpleErrorBanner "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Delete failed: Blog post not found" (Aeson.object ["postId" .= postId])
              pure $ renderSimpleErrorBanner "Blog post not found."
            Right (Just blogPost) -> do
              -- Check authorization: must be host of the show or author
              let isAuthor = blogPost.authorId == user.mId
              isHost <- checkIfHost userMetadata user showModel.id

              if isAuthor || isHost
                then deleteBlogPost blogPost
                else do
                  Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "postId" .= blogPost.id])
                  pure $ renderSimpleErrorBanner "You don't have permission to delete this blog post."

-- Helper for simple error banners
renderSimpleErrorBanner :: Text -> Lucid.Html ()
renderSimpleErrorBanner errorMsg =
  Lucid.div_
    [ Lucid.id_ "error-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "error-banner",
          Lucid.class_ "bg-red-100 border-2 border-red-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "⚠️"
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Delete Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#error-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              "✕"

deleteBlogPost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  ShowBlogPosts.Model ->
  m (Lucid.Html ())
deleteBlogPost blogPost = do
  execQuerySpan (ShowBlogPosts.deleteShowBlogPost blogPost.id) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "postId" .= blogPost.id])
      pure $ renderSimpleErrorBanner "Failed to delete blog post due to a database error."
    Right Nothing -> do
      Log.logInfo "Delete failed: Blog post not found during delete" (Aeson.object ["postId" .= blogPost.id])
      pure $ renderSimpleErrorBanner "Blog post not found during delete operation."
    Right (Just _) -> do
      Log.logInfo "Blog post deleted successfully" (Aeson.object ["postId" .= blogPost.id])
      -- Return empty response so the post card gets removed
      pure $ Lucid.toHtmlRaw ("" :: Text)

checkIfHost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  UserMetadata.Model ->
  User.Model ->
  Shows.Id ->
  m Bool
checkIfHost userMetadata user showId = do
  -- Admins don't need explicit host check since they have access to all shows
  if UserMetadata.isAdmin userMetadata.mUserRole
    then pure True
    else do
      result <- execQuerySpan (ShowHost.isUserHostOfShow user.mId showId)
      case result of
        Left _ -> pure False
        Right authorized -> pure authorized
