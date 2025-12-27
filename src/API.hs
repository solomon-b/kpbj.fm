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
    DashboardShowsRoutes (..),
    DashboardUsersRoutes (..),

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
import API.Dashboard.Episodes.Get.Handler qualified as Dashboard.Episodes.Get
import API.Dashboard.Episodes.Redirect.Handler qualified as Dashboard.Episodes.Redirect
import API.Dashboard.Episodes.Slug.Edit.Get.Handler qualified as Dashboard.Episodes.Slug.Edit.Get
import API.Dashboard.Episodes.Slug.Edit.Post.Handler qualified as Dashboard.Episodes.Slug.Edit.Post
import API.Dashboard.Episodes.Slug.Get.Handler qualified as Dashboard.Episodes.Slug.Get
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
import API.Dashboard.Users.Delete.Handler qualified as Dashboard.Users.Delete
import API.Dashboard.Users.Detail.Get.Handler qualified as Dashboard.Users.Detail.Get
import API.Dashboard.Users.Edit.Get.Handler qualified as Dashboard.Users.Edit.Get
import API.Dashboard.Users.Edit.Post.Handler qualified as Dashboard.Users.Edit.Post
import API.Dashboard.Users.Get.Handler qualified as Dashboard.Users.Get
import API.Dashboard.Users.Role.Patch.Handler qualified as Dashboard.Users.Role.Patch
import API.Dashboard.Users.Suspend.Post.Handler qualified as Dashboard.Users.Suspend.Post
import API.Dashboard.Users.Unsuspend.Post.Handler qualified as Dashboard.Users.Unsuspend.Post
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
import API.Shows.Slug.Episode.Delete.Handler qualified as Episodes.Delete
import API.Shows.Slug.Episode.DiscardDraft.Handler qualified as Episodes.DiscardDraft
import API.Shows.Slug.Episode.Get.Handler qualified as Episodes.Get
import API.Shows.Slug.Episode.Publish.Post.Handler qualified as Episodes.Publish.Post
import API.Shows.Slug.Get.Handler qualified as Show.Get
import API.Static.Get.Handler qualified as Static.Get
import API.TermsOfService.Get.Handler qualified as TermsOfService.Get
import API.Types
import API.User.Login.Get.Handler qualified as User.Login.Get
import API.User.Login.Post.Handler qualified as User.Login.Post
import API.User.Logout.Get.Handler qualified as User.Logout.Get
import API.User.Logout.Post.Handler qualified as User.Logout.Post
import API.User.Register.Get.Handler qualified as User.Register.Get
import API.User.Register.Post.Handler qualified as User.Register.Post
import App qualified
import App.Config (Environment)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Hasql.Pool qualified as HSQL.Pool
import Log (MonadLog)
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

runApi :: IO ()
runApi = App.runApp @API server ()

--------------------------------------------------------------------------------

server ::
  ( Has Tracer env,
    MonadCatch m,
    MonadLog m,
    MonadUnliftIO m,
    MonadClock m,
    MonadDB m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Environment env
  ) =>
  Environment ->
  Servant.ServerT API m
server env =
  Routes
    { rootGet = Root.Get.handler,
      staticGet = Static.Get.handler env,
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
      dashboard = dashboardRoutes
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
        { detail = Episodes.Get.handler,
          delete = Episodes.Delete.handler,
          discardDraft = Episodes.DiscardDraft.handler,
          publish = Episodes.Publish.Post.handler
        }

    userRoutes =
      UserRoutes
        { loginGet = User.Login.Get.handler,
          loginPost = User.Login.Post.handler,
          logoutGet = User.Logout.Get.handler,
          logoutPost = User.Logout.Post.handler,
          registerGet = User.Register.Get.handler,
          registerPost = User.Register.Post.handler
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
          blogs = dashboardBlogsRoutes
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
          editPost = Dashboard.Episodes.Slug.Edit.Post.handler
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
