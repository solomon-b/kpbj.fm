{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API where

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Archive.Get qualified as Archive.Get
import API.Blog.Get qualified as Blog.Get
import API.Blog.Post.Get qualified as Blog.Post.Get
import API.Dashboard.Blogs.Get qualified as Dashboard.Blogs.Get
import API.Dashboard.Blogs.Slug.Get qualified as Dashboard.Blogs.Slug.Get
import API.Dashboard.Episodes.Get qualified as Dashboard.Episodes.Get
import API.Dashboard.Episodes.Redirect qualified as Dashboard.Episodes.Redirect
import API.Dashboard.Episodes.Slug.Edit.Get qualified as Dashboard.Episodes.Slug.Edit.Get
import API.Dashboard.Episodes.Slug.Edit.Post qualified as Dashboard.Episodes.Slug.Edit.Post
import API.Dashboard.Episodes.Slug.Get qualified as Dashboard.Episodes.Slug.Get
import API.Dashboard.Events.Get qualified as Dashboard.Events.Get
import API.Dashboard.Events.New.Get qualified as Dashboard.Events.New.Get
import API.Dashboard.Events.New.Post qualified as Dashboard.Events.New.Post
import API.Dashboard.Events.Slug.Delete qualified as Dashboard.Events.Slug.Delete
import API.Dashboard.Events.Slug.Edit.Get qualified as Dashboard.Events.Slug.Edit.Get
import API.Dashboard.Events.Slug.Edit.Post qualified as Dashboard.Events.Slug.Edit.Post
import API.Dashboard.Events.Slug.Get qualified as Dashboard.Events.Slug.Get
import API.Dashboard.Get qualified as Dashboard.Get
import API.Dashboard.Shows.Get qualified as Dashboard.Shows.Get
import API.Dashboard.Shows.New.Get qualified as Dashboard.Shows.New.Get
import API.Dashboard.Shows.New.Post qualified as Dashboard.Shows.New.Post
import API.Dashboard.Shows.Slug.Edit.Get qualified as Dashboard.Shows.Slug.Edit.Get
import API.Dashboard.Shows.Slug.Edit.Post qualified as Dashboard.Shows.Slug.Edit.Post
import API.Dashboard.Shows.Slug.Get qualified as Dashboard.Shows.Slug.Get
import API.Dashboard.StationBlog.Get qualified as Dashboard.StationBlog.Get
import API.Dashboard.StationBlog.New.Get qualified as Dashboard.StationBlog.New.Get
import API.Dashboard.StationBlog.New.Post qualified as Dashboard.StationBlog.New.Post
import API.Dashboard.StationBlog.Slug.Delete qualified as Dashboard.StationBlog.Slug.Delete
import API.Dashboard.StationBlog.Slug.Edit.Get qualified as Dashboard.StationBlog.Slug.Edit.Get
import API.Dashboard.StationBlog.Slug.Edit.Post qualified as Dashboard.StationBlog.Slug.Edit.Post
import API.Dashboard.StationBlog.Slug.Get qualified as Dashboard.StationBlog.Slug.Get
import API.Dashboard.Users.Delete qualified as Dashboard.Users.Delete
import API.Dashboard.Users.Detail.Get qualified as Dashboard.Users.Detail.Get
import API.Dashboard.Users.Edit.Get qualified as Dashboard.Users.Edit.Get
import API.Dashboard.Users.Edit.Post qualified as Dashboard.Users.Edit.Post
import API.Dashboard.Users.Get qualified as Dashboard.Users.Get
import API.Dashboard.Users.Role.Patch qualified as Dashboard.Users.Role.Patch
import API.Dashboard.Users.Suspend.Post qualified as Dashboard.Users.Suspend.Post
import API.Dashboard.Users.Unsuspend.Post qualified as Dashboard.Users.Unsuspend.Post
import API.Donate.Get qualified as Donate.Get
import API.Events.Event.Get qualified as Events.Event.Get
import API.Events.Get qualified as Events.Get
import API.Get qualified as Root.Get
import API.Media.Get qualified as Media.Get
import API.PrivacyPolicy.Get qualified as PrivacyPolicy.Get
import API.Shows.Get qualified as Shows.Get
import API.Shows.Schedule.Get qualified as Shows.Schedule.Get
import API.Shows.Slug.Blog.Delete qualified as Show.Blog.Delete
import API.Shows.Slug.Blog.Edit.Get qualified as Show.Blog.Edit.Get
import API.Shows.Slug.Blog.Edit.Post qualified as Show.Blog.Edit.Post
import API.Shows.Slug.Blog.Get qualified as Show.Blog.Get
import API.Shows.Slug.Blog.New.Get qualified as Show.Blog.New.Get
import API.Shows.Slug.Blog.New.Post qualified as Show.Blog.New.Post
import API.Shows.Slug.Blog.Post.Get qualified as Show.Blog.Post.Get
import API.Shows.Slug.Episode.Delete qualified as Episodes.Delete
import API.Shows.Slug.Episode.DiscardDraft qualified as Episodes.DiscardDraft
import API.Shows.Slug.Episode.Get qualified as Episodes.Get
import API.Shows.Slug.Episode.New.Get qualified as Episodes.New.Get
import API.Shows.Slug.Episode.New.Post qualified as Episodes.New.Post
import API.Shows.Slug.Episode.Publish.Post qualified as Episodes.Publish.Post
import API.Shows.Slug.Get qualified as Show.Get
import API.Static.Get qualified as Static.Get
import API.TermsOfService.Get qualified as TermsOfService.Get
import API.User.Login.Get qualified as User.Login.Get
import API.User.Login.Post qualified as User.Login.Post
import API.User.Logout.Get qualified as User.Logout.Get
import API.User.Logout.Post qualified as User.Logout.Post
import API.User.Register.Get qualified as User.Register.Get
import API.User.Register.Post qualified as User.Register.Post
import App qualified
import App.Config (Environment)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.Filter (Filter)
import Domain.Types.FullName (FullName)
import Domain.Types.Genre (Genre)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.PageView (PageView)
import Domain.Types.Search (Search)
import Domain.Types.Slug (Slug)
import Domain.Types.UserSortBy (UserSortBy)
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL.Pool
import Log (MonadLog)
import OpenTelemetry.Trace (Tracer)
import Servant (NamedRoutes, (:-))
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

runApi :: IO ()
runApi = App.runApp @API server ()

--------------------------------------------------------------------------------

type API = NamedRoutes Routes

-- | Top-level API routes for KPBJ 95.9FM website.
--
-- Includes standalone public pages and nested route groups for blog, events,
-- shows, user authentication, and the admin dashboard.
data Routes mode = Routes
  { -- | @GET /@ - Home page
    rootGet :: mode :- Root.Get.Route
  , -- | @GET /static@ - Static file serving
    staticGet :: mode :- Static.Get.Route
  , -- | @GET /media@ - Media file serving
    mediaGet :: mode :- Media.Get.Route
  , -- | @GET /about@ - About page
    aboutGet :: mode :- About.Get.Route
  , -- | @GET /archive@ - Episode archive with search and filtering
    archiveGet :: mode :- Archive.Get.Route
  , -- | @GET /donate@ - Donation page
    donateGet :: mode :- Donate.Get.Route
  , -- | @GET /privacy-policy@ - Privacy policy page
    privacyPolicyGet :: mode :- PrivacyPolicy.Get.Route
  , -- | @GET /terms-of-service@ - Terms of service page
    termsOfServiceGet :: mode :- TermsOfService.Get.Route
  , -- | @/blog/...@ - Blog routes
    blog :: mode :- NamedRoutes BlogRoutes
  , -- | @/events/...@ - Events routes
    events :: mode :- NamedRoutes EventsRoutes
  , -- | @/shows/...@ - Shows routes
    shows :: mode :- NamedRoutes ShowsRoutes
  , -- | @/user/...@ - User authentication routes
    user :: mode :- NamedRoutes UserRoutes
  , -- | @/dashboard/...@ - Admin dashboard routes
    dashboard :: mode :- NamedRoutes DashboardRoutes
  }
  deriving stock (Generic)

-- | Station blog routes under @/blog@.
--
-- Provides listing and detail views for official KPBJ station blog posts.
data BlogRoutes mode = BlogRoutes
  { -- | @GET /blog@ - Blog listing with pagination and search
    list :: mode :- Blog.Get.Route
  , -- | @GET /blog/:id/:slug@ - Blog post detail (canonical URL with slug)
    postWithSlug :: mode :- Blog.Post.Get.RouteWithSlug
  , -- | @GET /blog/:id@ - Blog post detail (redirects to canonical URL)
    postWithoutSlug :: mode :- Blog.Post.Get.RouteWithoutSlug
  }
  deriving stock (Generic)

-- | Community events routes under @/events@.
--
-- Provides calendar views and detail pages for community events.
data EventsRoutes mode = EventsRoutes
  { -- | @GET /events@ - Events calendar with week/month/list views
    list :: mode :- Events.Get.Route
  , -- | @GET /events/:id/:slug@ - Event detail (canonical URL with slug)
    detailWithSlug :: mode :- Events.Event.Get.RouteWithSlug
  , -- | @GET /events/:id@ - Event detail (redirects to canonical URL)
    detailWithoutSlug :: mode :- Events.Event.Get.RouteWithoutSlug
  }
  deriving stock (Generic)

-- | Radio shows routes under @/shows@.
--
-- Includes show listings, schedule, individual show pages, and nested routes
-- for show-specific blog posts and episodes.
data ShowsRoutes mode = ShowsRoutes
  { -- | @GET /shows@ - Shows listing with genre and status filtering
    list :: mode :- Shows.Get.Route
  , -- | @GET /shows/schedule@ - Weekly show schedule
    schedule :: mode :- Shows.Schedule.Get.Route
  , -- | @GET /shows/:slug@ - Individual show page
    detail :: mode :- Show.Get.Route
  , -- | @/shows/:slug/blog/...@ - Show-specific blog routes
    blog :: mode :- NamedRoutes ShowBlogRoutes
  , -- | @/shows/:slug/episodes/...@ - Show episode routes
    episodes :: mode :- NamedRoutes ShowEpisodesRoutes
  }
  deriving stock (Generic)

-- | Show-specific blog routes under @/shows/:showSlug/blog@.
--
-- Allows hosts to create and manage blog posts for their shows.
data ShowBlogRoutes mode = ShowBlogRoutes
  { -- | @GET /shows/:showSlug/blog@ - Show blog listing
    list :: mode :- Show.Blog.Get.Route
  , -- | @GET /shows/:showSlug/blog/new@ - New blog post form
    newGet :: mode :- Show.Blog.New.Get.Route
  , -- | @POST /shows/:showSlug/blog/new@ - Create new blog post
    newPost :: mode :- Show.Blog.New.Post.Route
  , -- | @GET /shows/:showSlug/blog/:id/:slug@ - Blog post detail (canonical)
    postWithSlug :: mode :- Show.Blog.Post.Get.RouteWithSlug
  , -- | @GET /shows/:showSlug/blog/:id@ - Blog post detail (redirects)
    postWithoutSlug :: mode :- Show.Blog.Post.Get.RouteWithoutSlug
  , -- | @GET /shows/:showSlug/blog/:id/edit@ - Edit blog post form
    editGet :: mode :- Show.Blog.Edit.Get.Route
  , -- | @POST /shows/:showSlug/blog/:id/edit@ - Update blog post
    editPost :: mode :- Show.Blog.Edit.Post.Route
  , -- | @DELETE /shows/:showSlug/blog/:id@ - Delete blog post
    delete :: mode :- Show.Blog.Delete.Route
  }
  deriving stock (Generic)

-- | Show episode routes under @/shows/:showSlug/episodes@.
--
-- Allows hosts to create, view, and manage episodes for their shows.
data ShowEpisodesRoutes mode = ShowEpisodesRoutes
  { -- | @GET /shows/:showSlug/episodes/new@ - New episode upload form
    newGet :: mode :- Episodes.New.Get.Route
  , -- | @POST /shows/:showSlug/episodes/new@ - Create new episode
    newPost :: mode :- Episodes.New.Post.Route
  , -- | @GET /shows/:showSlug/episodes/:id/:slug@ - Episode detail (canonical)
    detailWithSlug :: mode :- Episodes.Get.RouteWithSlug
  , -- | @GET /shows/:showSlug/episodes/:id@ - Episode detail (redirects)
    detailWithoutSlug :: mode :- Episodes.Get.RouteWithoutSlug
  , -- | @DELETE /shows/:showSlug/episodes/:id@ - Delete episode
    delete :: mode :- Episodes.Delete.Route
  , -- | @POST /shows/:showSlug/episodes/:id/discard-draft@ - Discard draft episode
    discardDraft :: mode :- Episodes.DiscardDraft.Route
  , -- | @POST /shows/:showSlug/episodes/:id/publish@ - Publish draft episode
    publish :: mode :- Episodes.Publish.Post.Route
  }
  deriving stock (Generic)

-- | User authentication routes under @/user@.
--
-- Handles login, logout, and registration flows.
data UserRoutes mode = UserRoutes
  { -- | @GET /user/login@ - Login page
    loginGet :: mode :- User.Login.Get.Route
  , -- | @POST /user/login@ - Process login credentials
    loginPost :: mode :- User.Login.Post.Route
  , -- | @GET /user/logout@ - Logout confirmation page
    logoutGet :: mode :- User.Logout.Get.Route
  , -- | @POST /user/logout@ - Process logout
    logoutPost :: mode :- User.Logout.Post.Route
  , -- | @GET /user/register@ - Registration page
    registerGet :: mode :- User.Register.Get.Route
  , -- | @POST /user/register@ - Process registration
    registerPost :: mode :- User.Register.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard routes under @/dashboard@.
--
-- Provides management interfaces for episodes, blogs, events, shows, and users.
-- Access is restricted based on user roles.
data DashboardRoutes mode = DashboardRoutes
  { -- | @GET /dashboard@ - Dashboard home with stats and recent activity
    home :: mode :- Dashboard.Get.Route
  , -- | @GET /dashboard/episodes@ - Redirect to episodes list
    episodesRedirect :: mode :- Dashboard.Episodes.Redirect.Route
  , -- | Host-accessible dashboard routes (episodes, blogs, events)
    host :: mode :- NamedRoutes DashboardHostRoutes
  , -- | Admin-only dashboard routes (station blog, shows, users)
    admin :: mode :- NamedRoutes DashboardAdminRoutes
  }
  deriving stock (Generic)

-- | Host-accessible dashboard routes.
--
-- Routes for hosts to manage their show's episodes and blog posts.
data DashboardHostRoutes mode = DashboardHostRoutes
  { -- | @/dashboard/episodes/...@ - Episode management routes
    episodes :: mode :- NamedRoutes DashboardEpisodesRoutes
  , -- | @/dashboard/blogs/...@ - Show blog management routes
    blogs :: mode :- NamedRoutes DashboardBlogsRoutes
  }
  deriving stock (Generic)

-- | Admin-only dashboard routes.
--
-- Routes for admins to manage station blog, shows, events, and users.
data DashboardAdminRoutes mode = DashboardAdminRoutes
  { -- | @/dashboard/station-blog/...@ - Station blog management routes
    stationBlog :: mode :- NamedRoutes DashboardStationBlogRoutes
  , -- | @/dashboard/shows/...@ - Show management routes
    shows :: mode :- NamedRoutes DashboardShowsRoutes
  , -- | @/dashboard/events/...@ - Event management routes
    events :: mode :- NamedRoutes DashboardEventsRoutes
  , -- | @/dashboard/users/...@ - User management routes
    users :: mode :- NamedRoutes DashboardUsersRoutes
  }
  deriving stock (Generic)

-- | Dashboard episode management routes under @/dashboard/episodes@.
data DashboardEpisodesRoutes mode = DashboardEpisodesRoutes
  { -- | @GET /dashboard/episodes/:showSlug@ - Episode list for a show
    list :: mode :- Dashboard.Episodes.Get.Route
  , -- | @GET /dashboard/episodes/:showSlug/:episodeSlug@ - Episode detail
    detail :: mode :- Dashboard.Episodes.Slug.Get.Route
  , -- | @GET /dashboard/episodes/:showSlug/:episodeSlug/edit@ - Edit episode form
    editGet :: mode :- Dashboard.Episodes.Slug.Edit.Get.Route
  , -- | @POST /dashboard/episodes/:showSlug/:episodeSlug/edit@ - Update episode
    editPost :: mode :- Dashboard.Episodes.Slug.Edit.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard show blog management routes under @/dashboard/blogs@.
data DashboardBlogsRoutes mode = DashboardBlogsRoutes
  { -- | @GET /dashboard/blogs/:showSlug@ - Blog post list for a show
    list :: mode :- Dashboard.Blogs.Get.Route
  , -- | @GET /dashboard/blogs/:showSlug/:postSlug@ - Blog post detail
    detail :: mode :- Dashboard.Blogs.Slug.Get.Route
  }
  deriving stock (Generic)

-- | Dashboard event management routes under @/dashboard/events@.
data DashboardEventsRoutes mode = DashboardEventsRoutes
  { -- | @GET /dashboard/events@ - Event list
    list :: mode :- Dashboard.Events.Get.Route
  , -- | @GET /dashboard/events/new@ - New event form
    newGet :: mode :- Dashboard.Events.New.Get.Route
  , -- | @POST /dashboard/events/new@ - Create event
    newPost :: mode :- Dashboard.Events.New.Post.Route
  , -- | @GET /dashboard/events/:slug@ - Event detail
    detail :: mode :- Dashboard.Events.Slug.Get.Route
  , -- | @GET /dashboard/events/:slug/edit@ - Edit event form
    editGet :: mode :- Dashboard.Events.Slug.Edit.Get.Route
  , -- | @POST /dashboard/events/:slug/edit@ - Update event
    editPost :: mode :- Dashboard.Events.Slug.Edit.Post.Route
  , -- | @DELETE /dashboard/events/:slug@ - Delete event
    delete :: mode :- Dashboard.Events.Slug.Delete.Route
  }
  deriving stock (Generic)

-- | Dashboard station blog management routes under @/dashboard/station-blog@.
--
-- For staff and admins to manage official station blog posts.
data DashboardStationBlogRoutes mode = DashboardStationBlogRoutes
  { -- | @GET /dashboard/station-blog@ - Station blog post list
    list :: mode :- Dashboard.StationBlog.Get.Route
  , -- | @GET /dashboard/station-blog/new@ - New station blog post form
    newGet :: mode :- Dashboard.StationBlog.New.Get.Route
  , -- | @POST /dashboard/station-blog/new@ - Create station blog post
    newPost :: mode :- Dashboard.StationBlog.New.Post.Route
  , -- | @GET /dashboard/station-blog/:slug@ - Station blog post detail
    detail :: mode :- Dashboard.StationBlog.Slug.Get.Route
  , -- | @GET /dashboard/station-blog/:slug/edit@ - Edit station blog post form
    editGet :: mode :- Dashboard.StationBlog.Slug.Edit.Get.Route
  , -- | @POST /dashboard/station-blog/:slug/edit@ - Update station blog post
    editPost :: mode :- Dashboard.StationBlog.Slug.Edit.Post.Route
  , -- | @DELETE /dashboard/station-blog/:slug@ - Delete station blog post
    delete :: mode :- Dashboard.StationBlog.Slug.Delete.Route
  }
  deriving stock (Generic)

-- | Dashboard show management routes under @/dashboard/shows@.
--
-- For admins to create and manage radio shows.
data DashboardShowsRoutes mode = DashboardShowsRoutes
  { -- | @GET /dashboard/shows@ - Show list
    list :: mode :- Dashboard.Shows.Get.Route
  , -- | @GET /dashboard/shows/new@ - New show form
    newGet :: mode :- Dashboard.Shows.New.Get.Route
  , -- | @POST /dashboard/shows/new@ - Create show
    newPost :: mode :- Dashboard.Shows.New.Post.Route
  , -- | @GET /dashboard/shows/:slug@ - Show detail
    detail :: mode :- Dashboard.Shows.Slug.Get.Route
  , -- | @GET /dashboard/shows/:slug/edit@ - Edit show form
    editGet :: mode :- Dashboard.Shows.Slug.Edit.Get.Route
  , -- | @POST /dashboard/shows/:slug/edit@ - Update show
    editPost :: mode :- Dashboard.Shows.Slug.Edit.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard user management routes under @/dashboard/users@.
--
-- Admin-only routes for managing user accounts, roles, and suspensions.
data DashboardUsersRoutes mode = DashboardUsersRoutes
  { -- | @GET /dashboard/users@ - User list with pagination and search
    list :: mode :- Dashboard.Users.Get.Route
  , -- | @GET /dashboard/users/:id@ - User detail
    detail :: mode :- Dashboard.Users.Detail.Get.Route
  , -- | @GET /dashboard/users/:id/edit@ - Edit user form
    editGet :: mode :- Dashboard.Users.Edit.Get.Route
  , -- | @POST /dashboard/users/:id/edit@ - Update user
    editPost :: mode :- Dashboard.Users.Edit.Post.Route
  , -- | @PATCH /dashboard/users/:id/role@ - Update user role
    rolePatch :: mode :- Dashboard.Users.Role.Patch.Route
  , -- | @POST /dashboard/users/:id/suspend@ - Suspend user
    suspendPost :: mode :- Dashboard.Users.Suspend.Post.Route
  , -- | @POST /dashboard/users/:id/unsuspend@ - Unsuspend user
    unsuspendPost :: mode :- Dashboard.Users.Unsuspend.Post.Route
  , -- | @DELETE /dashboard/users/:id@ - Delete user
    delete :: mode :- Dashboard.Users.Delete.Route
  }
  deriving stock (Generic)

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
    { rootGet = Root.Get.handler
    , staticGet = Static.Get.handler env
    , mediaGet = Media.Get.handler
    , aboutGet = About.Get.handler
    , archiveGet = Archive.Get.handler
    , donateGet = Donate.Get.handler
    , privacyPolicyGet = PrivacyPolicy.Get.handler
    , termsOfServiceGet = TermsOfService.Get.handler
    , blog = blogRoutes
    , events = eventsRoutes
    , shows = showsRoutes
    , user = userRoutes
    , dashboard = dashboardRoutes
    }
  where
    blogRoutes =
      BlogRoutes
        { list = Blog.Get.handler
        , postWithSlug = Blog.Post.Get.handlerWithSlug
        , postWithoutSlug = Blog.Post.Get.handlerWithoutSlug
        }

    eventsRoutes =
      EventsRoutes
        { list = Events.Get.handler
        , detailWithSlug = Events.Event.Get.handlerWithSlug
        , detailWithoutSlug = Events.Event.Get.handlerWithoutSlug
        }

    showsRoutes =
      ShowsRoutes
        { list = Shows.Get.handler
        , schedule = Shows.Schedule.Get.handler
        , detail = Show.Get.handler
        , blog = showBlogRoutes
        , episodes = showEpisodesRoutes
        }

    showBlogRoutes =
      ShowBlogRoutes
        { list = Show.Blog.Get.handler
        , newGet = Show.Blog.New.Get.handler
        , newPost = Show.Blog.New.Post.handler
        , postWithSlug = Show.Blog.Post.Get.handlerWithSlug
        , postWithoutSlug = Show.Blog.Post.Get.handlerWithoutSlug
        , editGet = Show.Blog.Edit.Get.handler
        , editPost = Show.Blog.Edit.Post.handler
        , delete = Show.Blog.Delete.handler
        }

    showEpisodesRoutes =
      ShowEpisodesRoutes
        { newGet = Episodes.New.Get.handler
        , newPost = Episodes.New.Post.handler
        , detailWithSlug = Episodes.Get.handlerWithSlug
        , detailWithoutSlug = Episodes.Get.handlerWithoutSlug
        , delete = Episodes.Delete.handler
        , discardDraft = Episodes.DiscardDraft.handler
        , publish = Episodes.Publish.Post.handler
        }

    userRoutes =
      UserRoutes
        { loginGet = User.Login.Get.handler
        , loginPost = User.Login.Post.handler
        , logoutGet = User.Logout.Get.handler
        , logoutPost = User.Logout.Post.handler
        , registerGet = User.Register.Get.handler
        , registerPost = User.Register.Post.handler
        }

    dashboardRoutes =
      DashboardRoutes
        { home = Dashboard.Get.handler
        , episodesRedirect = Dashboard.Episodes.Redirect.handler
        , host = dashboardHostRoutes
        , admin = dashboardAdminRoutes
        }

    dashboardHostRoutes =
      DashboardHostRoutes
        { episodes = dashboardEpisodesRoutes
        , blogs = dashboardBlogsRoutes
        }

    dashboardAdminRoutes =
      DashboardAdminRoutes
        { stationBlog = dashboardStationBlogRoutes
        , shows = dashboardShowsRoutes
        , events = dashboardEventsRoutes
        , users = dashboardUsersRoutes
        }

    dashboardEpisodesRoutes =
      DashboardEpisodesRoutes
        { list = Dashboard.Episodes.Get.handler
        , detail = Dashboard.Episodes.Slug.Get.handler
        , editGet = Dashboard.Episodes.Slug.Edit.Get.handler
        , editPost = Dashboard.Episodes.Slug.Edit.Post.handler
        }

    dashboardBlogsRoutes =
      DashboardBlogsRoutes
        { list = Dashboard.Blogs.Get.handler
        , detail = Dashboard.Blogs.Slug.Get.handler
        }

    dashboardEventsRoutes =
      DashboardEventsRoutes
        { list = Dashboard.Events.Get.handler
        , newGet = Dashboard.Events.New.Get.handler
        , newPost = Dashboard.Events.New.Post.handler
        , detail = Dashboard.Events.Slug.Get.handler
        , editGet = Dashboard.Events.Slug.Edit.Get.handler
        , editPost = Dashboard.Events.Slug.Edit.Post.handler
        , delete = Dashboard.Events.Slug.Delete.handler
        }

    dashboardStationBlogRoutes =
      DashboardStationBlogRoutes
        { list = Dashboard.StationBlog.Get.handler
        , newGet = Dashboard.StationBlog.New.Get.handler
        , newPost = Dashboard.StationBlog.New.Post.handler
        , detail = Dashboard.StationBlog.Slug.Get.handler
        , editGet = Dashboard.StationBlog.Slug.Edit.Get.handler
        , editPost = Dashboard.StationBlog.Slug.Edit.Post.handler
        , delete = Dashboard.StationBlog.Slug.Delete.handler
        }

    dashboardShowsRoutes =
      DashboardShowsRoutes
        { list = Dashboard.Shows.Get.handler
        , newGet = Dashboard.Shows.New.Get.handler
        , newPost = Dashboard.Shows.New.Post.handler
        , detail = Dashboard.Shows.Slug.Get.handler
        , editGet = Dashboard.Shows.Slug.Edit.Get.handler
        , editPost = Dashboard.Shows.Slug.Edit.Post.handler
        }

    dashboardUsersRoutes =
      DashboardUsersRoutes
        { list = Dashboard.Users.Get.handler
        , detail = Dashboard.Users.Detail.Get.handler
        , editGet = Dashboard.Users.Edit.Get.handler
        , editPost = Dashboard.Users.Edit.Post.handler
        , rolePatch = Dashboard.Users.Role.Patch.handler
        , suspendPost = Dashboard.Users.Suspend.Post.handler
        , unsuspendPost = Dashboard.Users.Unsuspend.Post.handler
        , delete = Dashboard.Users.Delete.handler
        }

--------------------------------------------------------------------------------

-- | Route: GET /
rootGetLink :: Links.Link
rootGetLink = Links.safeLink (Proxy @API) (Proxy @Root.Get.Route)

-- | Route: GET /static
staticGetLink :: Links.Link
staticGetLink = Links.safeLink (Proxy @API) (Proxy @Static.Get.Route)

-- | Route: GET /media
mediaGetLink :: Links.Link
mediaGetLink = Links.safeLink (Proxy @API) (Proxy @Media.Get.Route)

-- | Route: GET /about
aboutGetLink :: Links.Link
aboutGetLink = Links.safeLink (Proxy @API) (Proxy @About.Get.Route)

-- | Route: GET /archive
archiveGetLink :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int64 -> Links.Link
archiveGetLink = Links.safeLink (Proxy @API) (Proxy @Archive.Get.Route)

-- | Route: GET /blog
blogGetLink :: Maybe Int64 -> Maybe Text -> Links.Link
blogGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Get.Route)

-- | Route: GET /blog/:id/:slug
blogPostGetLink :: BlogPosts.Id -> Slug -> Links.Link
blogPostGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Post.Get.RouteWithSlug)

-- | Route: GET /blog/:id (without slug, redirects to canonical)
blogPostGetLinkById :: BlogPosts.Id -> Links.Link
blogPostGetLinkById = Links.safeLink (Proxy @API) (Proxy @Blog.Post.Get.RouteWithoutSlug)

-- | Route: GET /donate
donateGetLink :: Links.Link
donateGetLink = Links.safeLink (Proxy @API) (Proxy @Donate.Get.Route)

-- | Route: GET /user/login
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userLoginGetLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Get.Route)

-- | Route: POST /user/login
userLoginPostLink :: Maybe Text -> Links.Link
userLoginPostLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Post.Route)

-- | Route: GET /user/logout
userLogoutGetLink :: Links.Link
userLogoutGetLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Get.Route)

-- | Route: POST /user/logout
userLogoutPostLink :: Links.Link
userLogoutPostLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Post.Route)

-- | Route: GET /user/register
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> Links.Link
userRegisterGetLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Get.Route)

-- | Route: POST /user/register
userRegisterPostLink :: Links.Link
userRegisterPostLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Post.Route)

-- | Route: GET /dashboard/shows/new
dashboardShowsNewGetLink :: Links.Link
dashboardShowsNewGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.New.Get.Route)

-- | Route: POST /dashboard/shows/new
dashboardShowsNewPostLink :: Links.Link
dashboardShowsNewPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.New.Post.Route)

-- | Route: GET /dashboard/users/:id
dashboardUserDetailGetLink :: User.Id -> Links.Link
dashboardUserDetailGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Detail.Get.Route)

-- | Route: GET /dashboard/users/:id/edit
dashboardUserEditGetLink :: User.Id -> Links.Link
dashboardUserEditGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Edit.Get.Route)

-- | Route: POST /dashboard/users/:id/edit
dashboardUserEditPostLink :: User.Id -> Links.Link
dashboardUserEditPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Edit.Post.Route)

-- | Route: PATCH /dashboard/users/:id/role
dashboardUserRolePatchLink :: User.Id -> Links.Link
dashboardUserRolePatchLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Role.Patch.Route)

-- | Route: DELETE /dashboard/users/:id
dashboardUserDeleteLink :: User.Id -> Links.Link
dashboardUserDeleteLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Delete.Route)

-- | Route: POST /dashboard/users/:id/suspend
dashboardUserSuspendPostLink :: User.Id -> Links.Link
dashboardUserSuspendPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Suspend.Post.Route)

-- | Route: POST /dashboard/users/:id/unsuspend
dashboardUserUnsuspendPostLink :: User.Id -> Links.Link
dashboardUserUnsuspendPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Unsuspend.Post.Route)

-- | Route: GET /privacy-policy
privacyPolicyGetLink :: Links.Link
privacyPolicyGetLink = Links.safeLink (Proxy @API) (Proxy @PrivacyPolicy.Get.Route)

-- | Route: GET /terms-of-service
termsOfServiceGetLink :: Links.Link
termsOfServiceGetLink = Links.safeLink (Proxy @API) (Proxy @TermsOfService.Get.Route)

-- | Route: GET /events
eventsGetLink :: Maybe Text -> Maybe PageView -> Links.Link
eventsGetLink = Links.safeLink (Proxy @API) (Proxy @Events.Get.Route)

-- | Route: GET /events/:id/:slug
eventGetLink :: Events.Id -> Slug -> Links.Link
eventGetLink = Links.safeLink (Proxy @API) (Proxy @Events.Event.Get.RouteWithSlug)

-- | Route: GET /events/:id (without slug, redirects to canonical)
eventGetLinkById :: Events.Id -> Links.Link
eventGetLinkById = Links.safeLink (Proxy @API) (Proxy @Events.Event.Get.RouteWithoutSlug)

-- | Route: GET /shows
showsGetLink :: Maybe PageNumber -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Links.Link
showsGetLink = Links.safeLink (Proxy @API) (Proxy @Shows.Get.Route)

-- | Route: GET /shows/schedule
showsScheduleGetLink :: Maybe WeekOffset -> Links.Link
showsScheduleGetLink = Links.safeLink (Proxy @API) (Proxy @Shows.Schedule.Get.Route)

-- | Route: GET /shows/:slug
showGetLink :: Slug -> Maybe Int -> Links.Link
showGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Get.Route)

-- | Route: GET /shows/:slug/blog
showBlogGetLink :: Slug -> Maybe Int64 -> Maybe Text -> Links.Link
showBlogGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Get.Route)

-- | Route: GET /shows/:show_id/blog/:post_id/:slug
showBlogPostGetLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogPostGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Post.Get.RouteWithSlug)

-- | Route: GET /shows/:show_id/blog/:post_id (without slug, redirects to canonical)
showBlogPostGetLinkById :: Shows.Id -> ShowBlogPosts.Id -> Links.Link
showBlogPostGetLinkById = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Post.Get.RouteWithoutSlug)

-- | Route: GET /shows/:slug/blog/new
showBlogNewGetLink :: Slug -> Links.Link
showBlogNewGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.New.Get.Route)

-- | Route: POST /shows/:slug/blog/new
showBlogNewPostLink :: Slug -> Links.Link
showBlogNewPostLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.New.Post.Route)

-- | Route: GET /shows/:show_id/blog/:post_id/:slug/edit
showBlogEditGetLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogEditGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Edit.Get.Route)

-- | Route: POST /shows/:show_id/blog/:post_id/:slug/edit
showBlogEditPostLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogEditPostLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Edit.Post.Route)

-- | Route: DELETE /shows/:show_id/:show_slug/blog/:post_id/:post_slug
showBlogDeleteLink :: Shows.Id -> Slug -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogDeleteLink = Links.safeLink (Proxy @API) (Proxy @Show.Blog.Delete.Route)

-- | Route: GET /dashboard/shows/:slug/edit
dashboardShowsSlugEditGetLink :: Slug -> Links.Link
dashboardShowsSlugEditGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.Slug.Edit.Get.Route)

-- | Route: POST /dashboard/shows/:slug/edit
dashboardShowsSlugEditPostLink :: Slug -> Links.Link
dashboardShowsSlugEditPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.Slug.Edit.Post.Route)

-- | Route: GET /dashboard/shows/:showId/:showSlug
dashboardShowsSlugGetLink :: Shows.Id -> Slug -> Maybe Int -> Links.Link
dashboardShowsSlugGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.Slug.Get.Route)

-- | Route: GET /shows/:show_slug/episodes/:episode_id/:slug
episodesGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesGetLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Get.RouteWithSlug)

-- | Route: GET /shows/:show_slug/episodes/:episode_id (without slug, redirects to canonical)
episodesGetLinkById :: Slug -> Episodes.Id -> Links.Link
episodesGetLinkById = Links.safeLink (Proxy @API) (Proxy @Episodes.Get.RouteWithoutSlug)

-- | Route: GET /shows/:show_slug/episodes/new
episodesNewGetLink :: Slug -> Links.Link
episodesNewGetLink = Links.safeLink (Proxy @API) (Proxy @Episodes.New.Get.Route)

-- | Route: POST /shows/:show_slug/episodes/new
episodesNewPostLink :: Slug -> Links.Link
episodesNewPostLink = Links.safeLink (Proxy @API) (Proxy @Episodes.New.Post.Route)

-- | Route: GET /dashboard/episodes/:show_slug/:episode_id/:slug/edit
dashboardEpisodeEditGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
dashboardEpisodeEditGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Slug.Edit.Get.Route)

-- | Route: POST /dashboard/episodes/:show_slug/:episode_id/:slug/edit
dashboardEpisodeEditPostLink :: Slug -> Episodes.Id -> Slug -> Links.Link
dashboardEpisodeEditPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Slug.Edit.Post.Route)

-- | Route: DELETE /shows/:show_slug/episodes/:episode_id/:episode_slug
-- Archive (soft delete) - staff+ only
episodesDeleteLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesDeleteLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Delete.Route)

-- | Route: DELETE /shows/:show_slug/episodes/:episode_id/:episode_slug/draft
-- Discard draft (hard delete) - hosts can discard their own drafts
episodesDiscardDraftLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesDiscardDraftLink = Links.safeLink (Proxy @API) (Proxy @Episodes.DiscardDraft.Route)

-- | Route: POST /shows/:show_slug/episodes/:episode_id/:episode_slug/publish
episodesPublishPostLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesPublishPostLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Publish.Post.Route)

-- | Route: GET /dashboard
hostDashboardGetLink :: Links.Link
hostDashboardGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Get.Route)

-- | Route: GET /dashboard/episodes (redirects to first show)
dashboardEpisodesRedirectLink :: Links.Link
dashboardEpisodesRedirectLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Redirect.Route)

-- | Route: GET /dashboard/episodes/:slug
dashboardEpisodesGetLink :: Slug -> Links.Link
dashboardEpisodesGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Get.Route)

-- | Route: GET /dashboard/episodes/:show_slug/:episode_id/:slug
dashboardEpisodeGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
dashboardEpisodeGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Slug.Get.Route)

-- | Route: GET /dashboard/blog
dashboardBlogsGetLink :: Slug -> Links.Link
dashboardBlogsGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Blogs.Get.Route)

-- | Route: GET /dashboard/blog/:show_id/:post_id/:slug
dashboardBlogPostGetLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
dashboardBlogPostGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Blogs.Slug.Get.Route)

-- | Route: GET /dashboard/users (no filters - for sidebar navigation)
dashboardUsersGetLink :: Links.Link
dashboardUsersGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Get.Route) Nothing Nothing Nothing Nothing

-- | Route: GET /dashboard/users (with pagination and filters)
dashboardUsersGetLinkFull :: Maybe Int64 -> Maybe (Filter Text) -> Maybe (Filter UserMetadata.UserRole) -> Maybe (Filter UserSortBy) -> Links.Link
dashboardUsersGetLinkFull = Links.safeLink (Proxy @API) (Proxy @Dashboard.Users.Get.Route)

-- | Route: GET /dashboard/shows (no filters - for sidebar navigation)
dashboardShowsGetLink :: Links.Link
dashboardShowsGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.Get.Route) Nothing Nothing Nothing

-- | Route: GET /dashboard/shows (with pagination and filters)
dashboardShowsGetLinkFull :: Maybe Int64 -> Maybe (Filter Text) -> Maybe (Filter Shows.Status) -> Links.Link
dashboardShowsGetLinkFull = Links.safeLink (Proxy @API) (Proxy @Dashboard.Shows.Get.Route)

-- | Route: GET /dashboard/station-blog (no pagination)
dashboardStationBlogGetLink :: Links.Link
dashboardStationBlogGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Get.Route) Nothing

-- | Route: GET /dashboard/station-blog (with pagination)
dashboardStationBlogGetLinkFull :: Maybe Int64 -> Links.Link
dashboardStationBlogGetLinkFull = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Get.Route)

-- | Route: GET /dashboard/station-blog/new
dashboardStationBlogNewGetLink :: Links.Link
dashboardStationBlogNewGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.New.Get.Route)

-- | Route: POST /dashboard/station-blog/new
dashboardStationBlogNewPostLink :: Links.Link
dashboardStationBlogNewPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.New.Post.Route)

-- | Route: GET /dashboard/station-blog/:id/:slug
dashboardStationBlogDetailGetLink :: BlogPosts.Id -> Slug -> Links.Link
dashboardStationBlogDetailGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Slug.Get.Route)

-- | Route: GET /dashboard/station-blog/:id/:slug/edit
dashboardStationBlogEditGetLink :: BlogPosts.Id -> Slug -> Links.Link
dashboardStationBlogEditGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Slug.Edit.Get.Route)

-- | Route: POST /dashboard/station-blog/:id/:slug/edit
dashboardStationBlogEditPostLink :: BlogPosts.Id -> Slug -> Links.Link
dashboardStationBlogEditPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Slug.Edit.Post.Route)

-- | Route: DELETE /dashboard/station-blog/:id/:slug
dashboardStationBlogDeleteLink :: BlogPosts.Id -> Slug -> Links.Link
dashboardStationBlogDeleteLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.StationBlog.Slug.Delete.Route)

-- | Route: GET /dashboard/events (no pagination)
dashboardEventsGetLink :: Links.Link
dashboardEventsGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Get.Route) Nothing

-- | Route: GET /dashboard/events (with pagination)
dashboardEventsGetLinkFull :: Maybe Int64 -> Links.Link
dashboardEventsGetLinkFull = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Get.Route)

-- | Route: GET /dashboard/events/new
dashboardEventsNewGetLink :: Links.Link
dashboardEventsNewGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.New.Get.Route)

-- | Route: POST /dashboard/events/new
dashboardEventsNewPostLink :: Links.Link
dashboardEventsNewPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.New.Post.Route)

-- | Route: GET /dashboard/events/:id/:slug
dashboardEventsDetailGetLink :: Events.Id -> Slug -> Links.Link
dashboardEventsDetailGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Slug.Get.Route)

-- | Route: GET /dashboard/events/:id/:slug/edit
dashboardEventsEditGetLink :: Events.Id -> Slug -> Links.Link
dashboardEventsEditGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Slug.Edit.Get.Route)

-- | Route: POST /dashboard/events/:id/:slug/edit
dashboardEventsEditPostLink :: Events.Id -> Slug -> Links.Link
dashboardEventsEditPostLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Slug.Edit.Post.Route)

-- | Route: DELETE /dashboard/events/:id/:slug
dashboardEventsDeleteLink :: Events.Id -> Slug -> Links.Link
dashboardEventsDeleteLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Events.Slug.Delete.Route)
