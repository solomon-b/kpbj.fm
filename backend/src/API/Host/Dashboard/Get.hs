{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeEditGetLink, episodeUploadGetLink, showsGetLink, userLoginGetLink)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.ShowBlog qualified as ShowBlog
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

episodeUploadGetUrl :: Links.URI
episodeUploadGetUrl = Links.linkURI episodeUploadGetLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /host/dashboard"
    ( "host"
        :> "dashboard"
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Host Dashboard template
template :: UserMetadata.Model -> [Show.ShowModel] -> [Episode.EpisodeModel] -> [ShowBlog.ShowBlogPostModel] -> Lucid.Html ()
template userMeta userShows recentEpisodes blogPosts = do
  -- Dashboard Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "HOST DASHBOARD"
        case userShows of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] "No shows assigned"
          (primaryShow : _) -> do
            Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
              Lucid.strong_ "Show: "
              Lucid.toHtml primaryShow.title
              " • "
              Lucid.strong_ "Host: "
              Lucid.toHtml userMeta.mDisplayName
            Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
              Lucid.strong_ "Schedule: "
              Lucid.span_ "TBD" -- TODO: Add schedule info
              " • "
              Lucid.strong_ "Genre: "
              maybe "TBD" Lucid.toHtml primaryShow.genre
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        case userShows of
          [] -> Lucid.span_ [Lucid.class_ "text-gray-400"] "No show assigned"
          (_primaryShow : _) -> do
            Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"
            -- TODO: Add link to show profile page when ready
            Lucid.span_ [Lucid.class_ "text-blue-300 text-sm"] "VIEW PUBLIC PAGE"

  -- Quick Actions
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "QUICK ACTIONS"
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-3 gap-4"] $ do
      Lucid.a_ [Lucid.href_ [i|/#{episodeUploadGetUrl}|], hxGet_ [i|/#{episodeUploadGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "bg-blue-600 text-white p-4 font-bold hover:bg-blue-700 transition-colors block text-center"] $ do
        "🎵 PREPARE SHOW"
      Lucid.a_ [Lucid.href_ "#new-blog-post", Lucid.class_ "bg-green-600 text-white p-4 font-bold hover:bg-green-700 transition-colors block text-center"] $ do
        "📝 NEW BLOG POST"
      Lucid.a_ [Lucid.href_ "#edit-profile", Lucid.class_ "bg-purple-600 text-white p-4 font-bold hover:bg-purple-700 transition-colors block text-center"] $ do
        "✏️ EDIT PROFILE"

  -- Main Dashboard Grid
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 mb-8 w-full"] $ do
    -- Recent Episodes Section
    Lucid.div_ [Lucid.class_ "lg:col-span-2"] $ do
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "RECENT EPISODES"

        case recentEpisodes of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
            Lucid.p_ "No episodes uploaded yet."
            Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Use 'PREPARE SHOW' to upload your first episode."
          _ -> Lucid.div_ [Lucid.class_ "space-y-4"] $ do
            mapM_ renderEpisodeCard $ take 3 recentEpisodes

      -- Recent Blog Posts
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-4"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold"] "RECENT BLOG POSTS"
          Lucid.button_ [Lucid.class_ "bg-green-600 text-white px-4 py-2 text-sm font-bold hover:bg-green-700"] $ do
            "NEW POST"

        case blogPosts of
          [] -> Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
            Lucid.p_ "No blog posts published yet."
            Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Share your thoughts with your audience!"
          _ -> Lucid.div_ [Lucid.class_ "space-y-4"] $ do
            mapM_ renderBlogPostCard $ take 3 blogPosts

    -- Sidebar
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      -- Show Stats
      renderStatsSection userShows recentEpisodes blogPosts

      -- Next Show Schedule
      renderScheduleSection userShows

-- | Render individual episode card
renderEpisodeCard :: Episode.EpisodeModel -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "border border-gray-300 p-4"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start mb-2"] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ "font-bold"] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
          Lucid.toHtml (show episode.createdAt)
          " • "
          -- TODO: Add duration when available
          "Duration TBD"
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        let episodeEditUrl = Links.linkURI $ episodeEditGetLink episode.id
        Lucid.a_ [Lucid.href_ [i|/#{episodeEditUrl}|], hxGet_ [i|/#{episodeEditUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "bg-gray-600 text-white px-3 py-1 text-xs font-bold hover:bg-gray-700 no-underline"] "EDIT"
        Lucid.button_ [Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"] "DELETE"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-700 mb-3"] $ do
      case episode.description of
        Nothing -> "No description available"
        Just desc -> do
          Lucid.toHtml $ Text.take 150 desc
          if Text.length desc > 150 then "..." else ""

    Lucid.div_ [Lucid.class_ "flex justify-between items-center text-xs text-gray-500"] $ do
      Lucid.div_ $ do
        "Status: "
        Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "Published"
      Lucid.div_ "👀 - views • 🎧 - downloads" -- TODO: Add real stats

-- | Render individual blog post card
renderBlogPostCard :: ShowBlog.ShowBlogPostModel -> Lucid.Html ()
renderBlogPostCard post = do
  Lucid.div_ [Lucid.class_ "border border-gray-300 p-4"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start mb-2"] $ do
      Lucid.div_ $ do
        Lucid.h3_ [Lucid.class_ "font-bold"] $ Lucid.toHtml post.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
          Lucid.toHtml (show post.createdAt)
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.button_ [Lucid.class_ "bg-gray-600 text-white px-3 py-1 text-xs font-bold hover:bg-gray-700"] "EDIT"
        Lucid.button_ [Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"] "DELETE"

    Lucid.p_ [Lucid.class_ "text-sm text-gray-700 mb-2"] $ do
      let content = post.content
      Lucid.toHtml $ Text.take 120 content
      if Text.length content > 120 then "..." else ""

    Lucid.div_ [Lucid.class_ "flex justify-between items-center text-xs text-gray-500"] $ do
      Lucid.div_ $ do
        "Status: "
        Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "Published"
      Lucid.div_ "💬 - comments • 👀 - views" -- TODO: Add real stats

-- | Render stats sidebar section
renderStatsSection :: [Show.ShowModel] -> [Episode.EpisodeModel] -> [ShowBlog.ShowBlogPostModel] -> Lucid.Html ()
renderStatsSection userShows recentEpisodes blogPosts = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "SHOW STATISTICS"
    Lucid.div_ [Lucid.class_ "space-y-3 text-sm"] $ do
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Total Episodes:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length recentEpisodes))
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Total Downloads:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] "-" -- TODO: Add real download stats
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Blog Posts:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length blogPosts))
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] "Shows:"
        Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml (show (length userShows))

-- | Render schedule sidebar section
renderScheduleSection :: [Show.ShowModel] -> Lucid.Html ()
renderScheduleSection userShows = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-4"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "YOUR SCHEDULE"
    case userShows of
      [] -> Lucid.div_ [Lucid.class_ "text-center text-sm"] $ do
        Lucid.div_ [Lucid.class_ "text-gray-300 mb-4"] "No shows assigned yet."
        Lucid.div_ [Lucid.class_ "text-xs text-gray-400"] "Contact station management to get your show scheduled."
      (primaryShow : _) -> do
        Lucid.div_ [Lucid.class_ "text-center text-sm mb-4"] $ do
          Lucid.div_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml primaryShow.title
          Lucid.div_ [Lucid.class_ "mb-1"] "Schedule: TBD" -- TODO: Add schedule info
          Lucid.div_ [Lucid.class_ "text-xs text-gray-300"] "Set by station management"

        Lucid.div_ [Lucid.class_ "bg-gray-700 p-3 mb-4 text-xs"] $ do
          Lucid.div_ [Lucid.class_ "font-bold mb-2"] "NEXT SHOW:"
          Lucid.div_ "TBD - Schedule to be determined"
          Lucid.div_ [Lucid.class_ "text-gray-300"] "Contact management for scheduling"

        Lucid.div_ [Lucid.class_ "space-y-2"] $ do
          Lucid.button_ [Lucid.class_ "w-full bg-green-600 text-white py-2 text-xs font-bold hover:bg-green-700"] $ do
            "VIEW FULL SCHEDULE"

-- | Template for unauthorized access (non-hosts)
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "Only Host, Staff and Admin users can access the host dashboard."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO SHOWS"
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:blue-700 inline-block"
        ]
        "LOGIN"

-- | Template for users not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-6"] "Please login to access the host dashboard."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
      ]
      "LOGIN"

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

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
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  checkAuth isHtmxRequest cookie $ \user userMetadata -> do
    if UserMetadata.isHostOrHigher userMetadata.mUserRole
      then do
        Log.logInfo "Authorized user accessing host dashboard" userMetadata.mDisplayName

        -- Fetch user's shows
        userShows <- fromRightM (\_ -> pure []) $ execQuerySpan (Show.getShowsForUser (User.mId user))

        -- Fetch recent episodes for user's shows
        recentEpisodes <- case userShows of
          [] -> pure []
          (primaryShow : _) -> do
            episodesResult <- execQuerySpan (Episode.getEpisodesByShowId primaryShow.id)
            case episodesResult of
              Left _err -> pure []
              Right episodes -> pure episodes

        -- Fetch recent blog posts by this user
        blogPostsResult <- execQuerySpan (ShowBlog.getShowBlogPostsByAuthor (User.mId user) 10 0)
        blogPosts <- case blogPostsResult of
          Left _err -> pure []
          Right posts -> pure posts

        let dashboardTemplate = template userMetadata userShows recentEpisodes blogPosts
        renderTemplate isHtmxRequest (Just userMetadata) dashboardTemplate
      else do
        Log.logInfo "User without Host role tried to access host dashboard" userMetadata.mDisplayName
        renderTemplate isHtmxRequest (Just userMetadata) notAuthorizedTemplate

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

checkAuth ::
  ( MonadDB m,
    MonadCatch m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Bool ->
  Maybe Text ->
  (User.Model -> UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
checkAuth isHtmxRequest cookie k = do
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn -> do
      Log.logInfo "Unauthorized access to host dashboard" ()
      renderTemplate isHtmxRequest Nothing notLoggedInTemplate
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right (Just userMetadata) -> k user userMetadata
        _ -> do
          Log.logInfo "Failed to fetch user metadata for host dashboard" user.mId
          renderTemplate isHtmxRequest Nothing notLoggedInTemplate
