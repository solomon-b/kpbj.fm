{-# LANGUAGE DuplicateRecordFields #-}

module API
  ( -- * Server
    runApi,
    server,

    -- * Re-exports from API.Types
    API,
    Routes (..),
    BlogRoutes (..),
    EventsRoutes (..),
    ShowsRoutes (..),
    ShowBlogRoutes (..),
    ShowEpisodesRoutes (..),
    UserRoutes (..),
    DashboardRoutes (..),
    DashboardHostRoutes (..),
    DashboardAdminRoutes (..),
    DashboardEpisodesRoutes (..),
    DashboardBlogsRoutes (..),
    DashboardEventsRoutes (..),
    DashboardStationBlogRoutes (..),
    DashboardStationIdsRoutes (..),
    DashboardEphemeralUploadsRoutes (..),
    DashboardShowsRoutes (..),
    DashboardUsersRoutes (..),
    UploadRoutes (..),

    -- * Re-exports from API.Links (for backward compatibility)
    module API.Links,
  )
where

--------------------------------------------------------------------------------

import API.About.Get.Handler qualified as About.Get
import API.Blog.Get.Handler qualified as Blog.Get
import API.Blog.Post.Get.Handler qualified as Blog.Post.Get
import API.Dashboard.Blogs.Get.Handler qualified as Dashboard.Blogs.Get
import API.Dashboard.Blogs.New.Get.Handler qualified as Dashboard.Blogs.New.Get
import API.Dashboard.Blogs.New.Post.Handler qualified as Dashboard.Blogs.New.Post
import API.Dashboard.Blogs.Slug.Delete.Handler qualified as Dashboard.Blogs.Slug.Delete
import API.Dashboard.Blogs.Slug.Edit.Get.Handler qualified as Dashboard.Blogs.Slug.Edit.Get
import API.Dashboard.Blogs.Slug.Edit.Post.Handler qualified as Dashboard.Blogs.Slug.Edit.Post
import API.Dashboard.Blogs.Slug.Get.Handler qualified as Dashboard.Blogs.Slug.Get
import API.Dashboard.EphemeralUploads.Get.Handler qualified as Dashboard.EphemeralUploads.Get
import API.Dashboard.EphemeralUploads.Id.Delete.Handler qualified as Dashboard.EphemeralUploads.Id.Delete
import API.Dashboard.EphemeralUploads.New.Get.Handler qualified as Dashboard.EphemeralUploads.New.Get
import API.Dashboard.EphemeralUploads.New.Post.Handler qualified as Dashboard.EphemeralUploads.New.Post
import API.Dashboard.Episodes.Get.Handler qualified as Dashboard.Episodes.Get
import API.Dashboard.Episodes.Redirect.Handler qualified as Dashboard.Episodes.Redirect
import API.Dashboard.Episodes.Slug.Delete.Handler qualified as Dashboard.Episodes.Slug.Delete
import API.Dashboard.Episodes.Slug.DiscardDraft.Handler qualified as Dashboard.Episodes.Slug.DiscardDraft
import API.Dashboard.Episodes.Slug.Edit.Get.Handler qualified as Dashboard.Episodes.Slug.Edit.Get
import API.Dashboard.Episodes.Slug.Edit.Post.Handler qualified as Dashboard.Episodes.Slug.Edit.Post
import API.Dashboard.Episodes.Slug.Get.Handler qualified as Dashboard.Episodes.Slug.Get
import API.Dashboard.Episodes.Slug.Publish.Post.Handler qualified as Dashboard.Episodes.Slug.Publish.Post
import API.Dashboard.Events.Get.Handler qualified as Dashboard.Events.Get
import API.Dashboard.Events.New.Get.Handler qualified as Dashboard.Events.New.Get
import API.Dashboard.Events.New.Post.Handler qualified as Dashboard.Events.New.Post
import API.Dashboard.Events.Slug.Delete.Handler qualified as Dashboard.Events.Slug.Delete
import API.Dashboard.Events.Slug.Edit.Get.Handler qualified as Dashboard.Events.Slug.Edit.Get
import API.Dashboard.Events.Slug.Edit.Post.Handler qualified as Dashboard.Events.Slug.Edit.Post
import API.Dashboard.Events.Slug.Get.Handler qualified as Dashboard.Events.Slug.Get
import API.Dashboard.Get.Handler qualified as Dashboard.Get
import API.Dashboard.Profile.Edit.Get.Handler qualified as Dashboard.Profile.Edit.Get
import API.Dashboard.Profile.Edit.Post.Handler qualified as Dashboard.Profile.Edit.Post
import API.Dashboard.Shows.Get.Handler qualified as Dashboard.Shows.Get
import API.Dashboard.Shows.New.Get.Handler qualified as Dashboard.Shows.New.Get
import API.Dashboard.Shows.New.Post.Handler qualified as Dashboard.Shows.New.Post
import API.Dashboard.Shows.Slug.Edit.Get.Handler qualified as Dashboard.Shows.Slug.Edit.Get
import API.Dashboard.Shows.Slug.Edit.Post.Handler qualified as Dashboard.Shows.Slug.Edit.Post
import API.Dashboard.Shows.Slug.Episode.New.Get.Handler qualified as Dashboard.Shows.Slug.Episode.New.Get
import API.Dashboard.Shows.Slug.Episode.New.Post.Handler qualified as Dashboard.Shows.Slug.Episode.New.Post
import API.Dashboard.Shows.Slug.Get.Handler qualified as Dashboard.Shows.Slug.Get
import API.Dashboard.StationBlog.Get.Handler qualified as Dashboard.StationBlog.Get
import API.Dashboard.StationBlog.New.Get.Handler qualified as Dashboard.StationBlog.New.Get
import API.Dashboard.StationBlog.New.Post.Handler qualified as Dashboard.StationBlog.New.Post
import API.Dashboard.StationBlog.Slug.Delete.Handler qualified as Dashboard.StationBlog.Slug.Delete
import API.Dashboard.StationBlog.Slug.Edit.Get.Handler qualified as Dashboard.StationBlog.Slug.Edit.Get
import API.Dashboard.StationBlog.Slug.Edit.Post.Handler qualified as Dashboard.StationBlog.Slug.Edit.Post
import API.Dashboard.StationBlog.Slug.Get.Handler qualified as Dashboard.StationBlog.Slug.Get
import API.Dashboard.StationIds.Get.Handler qualified as Dashboard.StationIds.Get
import API.Dashboard.StationIds.Id.Delete.Handler qualified as Dashboard.StationIds.Id.Delete
import API.Dashboard.StationIds.New.Get.Handler qualified as Dashboard.StationIds.New.Get
import API.Dashboard.StationIds.New.Post.Handler qualified as Dashboard.StationIds.New.Post
import API.Dashboard.Users.Delete.Handler qualified as Dashboard.Users.Delete
import API.Dashboard.Users.Detail.Get.Handler qualified as Dashboard.Users.Detail.Get
import API.Dashboard.Users.Edit.Get.Handler qualified as Dashboard.Users.Edit.Get
import API.Dashboard.Users.Edit.Post.Handler qualified as Dashboard.Users.Edit.Post
import API.Dashboard.Users.Get.Handler qualified as Dashboard.Users.Get
import API.Dashboard.Users.Role.Patch.Handler qualified as Dashboard.Users.Role.Patch
import API.Dashboard.Users.Suspend.Post.Handler qualified as Dashboard.Users.Suspend.Post
import API.Dashboard.Users.Unsuspend.Post.Handler qualified as Dashboard.Users.Unsuspend.Post
import API.Debug.Version.Get.Handler qualified as Debug.Version.Get
import API.Donate.Get.Handler qualified as Donate.Get
import API.Events.Event.Get.Handler qualified as Events.Event.Get
import API.Events.Get.Handler qualified as Events.Get
import API.Get.Handler qualified as Root.Get
import API.Links
import API.Media.Get.Handler qualified as Media.Get
import API.PrivacyPolicy.Get.Handler qualified as PrivacyPolicy.Get
import API.Schedule.Get.Handler qualified as Schedule.Get
import API.Shows.Get.Handler qualified as Shows.Get
import API.Shows.Slug.Blog.Get.Handler qualified as Show.Blog.Get
import API.Shows.Slug.Blog.Post.Get.Handler qualified as Show.Blog.Post.Get
import API.Shows.Slug.Episode.Get.Handler qualified as Episodes.Get
import API.Shows.Slug.Get.Handler qualified as Show.Get
import API.Static.RangePng.Get.Handler qualified as Static.RangePng.Get
import API.TermsOfService.Get.Handler qualified as TermsOfService.Get
import API.Types
import API.Uploads.Audio.Post.Handler qualified as Uploads.Audio.Post
import API.User.ForgotPassword.Get.Handler qualified as User.ForgotPassword.Get
import API.User.ForgotPassword.Post.Handler qualified as User.ForgotPassword.Post
import API.User.Login.Get.Handler qualified as User.Login.Get
import API.User.Login.Post.Handler qualified as User.Login.Post
import API.User.Logout.Get.Handler qualified as User.Logout.Get
import API.User.Logout.Post.Handler qualified as User.Logout.Post
import API.User.Register.Get.Handler qualified as User.Register.Get
import API.User.Register.Post.Handler qualified as User.Register.Post
import API.User.ResetPassword.Get.Handler qualified as User.ResetPassword.Get
import API.User.ResetPassword.Post.Handler qualified as User.ResetPassword.Post
import API.User.VerifyEmail.Get.Handler qualified as User.VerifyEmail.Get
import API.User.VerifyEmailResend.Post.Handler qualified as User.VerifyEmailResend.Post
import API.User.VerifyEmailSent.Get.Handler qualified as User.VerifyEmailSent.Get
import App qualified
import App.Config (Environment (..))
import App.CustomContext (initCustomContext)
import App.Monad (AppM)
import Control.Concurrent.Async qualified as Async
import Data.Has qualified as Has
import Data.Maybe (fromMaybe)
import Effects.BackgroundJobs qualified as BackgroundJobs
import Hasql.Pool qualified as HSQL.Pool
import Servant qualified
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- | Run the API server with the custom context.
--
-- Uses 'App.withAppResources' to acquire the database pool and other resources,
-- then spawns background cleanup jobs alongside the main server using the shared pool.
runApi :: IO ()
runApi = do
  env <- loadEnvironment
  customCtx <- initCustomContext env
  App.withAppResources customCtx $ \appCtx -> do
    -- Extract the shared database pool from the app context
    let pool = Has.getter @HSQL.Pool.Pool appCtx

    -- Load cleanup interval from environment
    cleanupInterval <- loadCleanupInterval

    -- Start background jobs, then run the main server.
    -- When the server exits, withAsync automatically cancels the background task.
    -- Note: If the server is terminated mid-cleanup (e.g., SIGTERM), the current
    -- query may be interrupted. This is acceptable since cleanup queries are
    -- idempotent DELETEs that will run again on next startup.
    Async.withAsync (BackgroundJobs.runCleanupLoop cleanupInterval pool) $ \_cleanupJob -> do
      App.runServer @API server appCtx

-- | Load environment from APP_ENVIRONMENT env var.
loadEnvironment :: IO Environment
loadEnvironment = do
  mEnv <- lookupEnv "APP_ENVIRONMENT"
  pure $ case mEnv of
    Just "Production" -> Production
    _ -> Development

-- | Load cleanup interval from APP_CLEANUP_INTERVAL_SECONDS env var.
--
-- Falls back to 'BackgroundJobs.defaultCleanupIntervalSeconds' (1 hour) if not set or invalid.
loadCleanupInterval :: IO Int
loadCleanupInterval = do
  mInterval <- lookupEnv "APP_CLEANUP_INTERVAL_SECONDS"
  pure $ fromMaybe BackgroundJobs.defaultCleanupIntervalSeconds (mInterval >>= readMaybe)

--------------------------------------------------------------------------------

server :: Environment -> Servant.ServerT API AppM
server env =
  Routes
    { rootGet = Root.Get.handler,
      staticRangePngGet = Static.RangePng.Get.handler,
      mediaGet = Media.Get.handler,
      aboutGet = About.Get.handler,
      donateGet = Donate.Get.handler,
      privacyPolicyGet = PrivacyPolicy.Get.handler,
      termsOfServiceGet = TermsOfService.Get.handler,
      blog = blogRoutes,
      events = eventsRoutes,
      schedule = Schedule.Get.handler,
      shows = showsRoutes,
      user = userRoutes,
      dashboard = dashboardRoutes,
      uploads = uploadRoutes,
      debugVersion = Debug.Version.Get.handler
    }
  where
    blogRoutes =
      BlogRoutes
        { list = Blog.Get.handler,
          postWithSlug = Blog.Post.Get.handlerWithSlug,
          postWithoutSlug = Blog.Post.Get.handlerWithoutSlug
        }

    eventsRoutes =
      EventsRoutes
        { list = Events.Get.handler,
          detailWithSlug = Events.Event.Get.handlerWithSlug,
          detailWithoutSlug = Events.Event.Get.handlerWithoutSlug
        }

    showsRoutes =
      ShowsRoutes
        { list = Shows.Get.handler,
          detail = Show.Get.handler,
          blog = showBlogRoutes,
          episodes = showEpisodesRoutes
        }

    showBlogRoutes =
      ShowBlogRoutes
        { list = Show.Blog.Get.handler,
          postWithSlug = Show.Blog.Post.Get.handlerWithSlug,
          postWithoutSlug = Show.Blog.Post.Get.handlerWithoutSlug
        }

    showEpisodesRoutes =
      ShowEpisodesRoutes
        { detail = Episodes.Get.handler
        }

    userRoutes =
      UserRoutes
        { loginGet = User.Login.Get.handler,
          loginPost = User.Login.Post.handler,
          logoutGet = User.Logout.Get.handler,
          logoutPost = User.Logout.Post.handler,
          registerGet = User.Register.Get.handler,
          registerPost = User.Register.Post.handler,
          verifyEmailGet = User.VerifyEmail.Get.handler,
          verifyEmailSentGet = User.VerifyEmailSent.Get.handler,
          verifyEmailResendPost = User.VerifyEmailResend.Post.handler,
          forgotPasswordGet = User.ForgotPassword.Get.handler,
          forgotPasswordPost = User.ForgotPassword.Post.handler,
          resetPasswordGet = User.ResetPassword.Get.handler,
          resetPasswordPost = User.ResetPassword.Post.handler
        }

    dashboardRoutes =
      DashboardRoutes
        { home = Dashboard.Get.handler,
          episodesRedirect = Dashboard.Episodes.Redirect.handler,
          profileEditGet = Dashboard.Profile.Edit.Get.handler,
          profileEditPost = Dashboard.Profile.Edit.Post.handler,
          host = dashboardHostRoutes,
          admin = dashboardAdminRoutes
        }

    dashboardHostRoutes =
      DashboardHostRoutes
        { episodes = dashboardEpisodesRoutes,
          blogs = dashboardBlogsRoutes,
          stationIds = dashboardStationIdsRoutes,
          ephemeralUploads = dashboardEphemeralUploadsRoutes
        }

    dashboardStationIdsRoutes =
      DashboardStationIdsRoutes
        { list = Dashboard.StationIds.Get.handler,
          newGet = Dashboard.StationIds.New.Get.handler,
          newPost = Dashboard.StationIds.New.Post.handler,
          delete = Dashboard.StationIds.Id.Delete.handler
        }

    dashboardEphemeralUploadsRoutes =
      DashboardEphemeralUploadsRoutes
        { list = Dashboard.EphemeralUploads.Get.handler,
          newGet = Dashboard.EphemeralUploads.New.Get.handler,
          newPost = Dashboard.EphemeralUploads.New.Post.handler,
          delete = Dashboard.EphemeralUploads.Id.Delete.handler
        }

    dashboardAdminRoutes =
      DashboardAdminRoutes
        { stationBlog = dashboardStationBlogRoutes,
          shows = dashboardShowsRoutes,
          events = dashboardEventsRoutes,
          users = dashboardUsersRoutes
        }

    dashboardEpisodesRoutes =
      DashboardEpisodesRoutes
        { list = Dashboard.Episodes.Get.handler,
          detail = Dashboard.Episodes.Slug.Get.handler,
          editGet = Dashboard.Episodes.Slug.Edit.Get.handler,
          editPost = Dashboard.Episodes.Slug.Edit.Post.handler,
          delete = Dashboard.Episodes.Slug.Delete.handler,
          discardDraft = Dashboard.Episodes.Slug.DiscardDraft.handler,
          publish = Dashboard.Episodes.Slug.Publish.Post.handler
        }

    dashboardBlogsRoutes =
      DashboardBlogsRoutes
        { list = Dashboard.Blogs.Get.handler,
          detail = Dashboard.Blogs.Slug.Get.handler,
          newGet = Dashboard.Blogs.New.Get.handler,
          newPost = Dashboard.Blogs.New.Post.handler,
          editGet = Dashboard.Blogs.Slug.Edit.Get.handler,
          editPost = Dashboard.Blogs.Slug.Edit.Post.handler,
          delete = Dashboard.Blogs.Slug.Delete.handler
        }

    dashboardEventsRoutes =
      DashboardEventsRoutes
        { list = Dashboard.Events.Get.handler,
          newGet = Dashboard.Events.New.Get.handler,
          newPost = Dashboard.Events.New.Post.handler,
          detail = Dashboard.Events.Slug.Get.handler,
          editGet = Dashboard.Events.Slug.Edit.Get.handler,
          editPost = Dashboard.Events.Slug.Edit.Post.handler,
          delete = Dashboard.Events.Slug.Delete.handler
        }

    dashboardStationBlogRoutes =
      DashboardStationBlogRoutes
        { list = Dashboard.StationBlog.Get.handler,
          newGet = Dashboard.StationBlog.New.Get.handler,
          newPost = Dashboard.StationBlog.New.Post.handler,
          detail = Dashboard.StationBlog.Slug.Get.handler,
          editGet = Dashboard.StationBlog.Slug.Edit.Get.handler,
          editPost = Dashboard.StationBlog.Slug.Edit.Post.handler,
          delete = Dashboard.StationBlog.Slug.Delete.handler
        }

    dashboardShowsRoutes =
      DashboardShowsRoutes
        { list = Dashboard.Shows.Get.handler,
          newGet = Dashboard.Shows.New.Get.handler,
          newPost = Dashboard.Shows.New.Post.handler,
          detail = Dashboard.Shows.Slug.Get.handler,
          editGet = Dashboard.Shows.Slug.Edit.Get.handler,
          editPost = Dashboard.Shows.Slug.Edit.Post.handler,
          episodeNewGet = Dashboard.Shows.Slug.Episode.New.Get.handler,
          episodeNewPost = Dashboard.Shows.Slug.Episode.New.Post.handler
        }

    dashboardUsersRoutes =
      DashboardUsersRoutes
        { list = Dashboard.Users.Get.handler,
          detail = Dashboard.Users.Detail.Get.handler,
          editGet = Dashboard.Users.Edit.Get.handler,
          editPost = Dashboard.Users.Edit.Post.handler,
          rolePatch = Dashboard.Users.Role.Patch.handler,
          suspendPost = Dashboard.Users.Suspend.Post.handler,
          unsuspendPost = Dashboard.Users.Unsuspend.Post.handler,
          delete = Dashboard.Users.Delete.handler
        }

    uploadRoutes =
      UploadRoutes
        { audioPost = Uploads.Audio.Post.handler
        }
