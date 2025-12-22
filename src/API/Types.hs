{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Types where

--------------------------------------------------------------------------------

import API.About.Get.Route qualified as About.Get
import API.Blog.Get.Route qualified as Blog.Get
import API.Blog.Post.Get.Route qualified as Blog.Post.Get
import API.Dashboard.Blogs.Get.Route qualified as Dashboard.Blogs.Get
import API.Dashboard.Blogs.Slug.Get.Route qualified as Dashboard.Blogs.Slug.Get
import API.Dashboard.Episodes.Get.Route qualified as Dashboard.Episodes.Get
import API.Dashboard.Episodes.Redirect.Route qualified as Dashboard.Episodes.Redirect
import API.Dashboard.Episodes.Slug.Edit.Get.Route qualified as Dashboard.Episodes.Slug.Edit.Get
import API.Dashboard.Episodes.Slug.Edit.Post.Route qualified as Dashboard.Episodes.Slug.Edit.Post
import API.Dashboard.Episodes.Slug.Get.Route qualified as Dashboard.Episodes.Slug.Get
import API.Dashboard.Events.Get.Route qualified as Dashboard.Events.Get
import API.Dashboard.Events.New.Get.Route qualified as Dashboard.Events.New.Get
import API.Dashboard.Events.New.Post.Route qualified as Dashboard.Events.New.Post
import API.Dashboard.Events.Slug.Delete.Route qualified as Dashboard.Events.Slug.Delete
import API.Dashboard.Events.Slug.Edit.Get.Route qualified as Dashboard.Events.Slug.Edit.Get
import API.Dashboard.Events.Slug.Edit.Post.Route qualified as Dashboard.Events.Slug.Edit.Post
import API.Dashboard.Events.Slug.Get.Route qualified as Dashboard.Events.Slug.Get
import API.Dashboard.Get.Route qualified as Dashboard.Get
import API.Dashboard.Profile.Edit.Get.Route qualified as Dashboard.Profile.Edit.Get
import API.Dashboard.Profile.Edit.Post.Route qualified as Dashboard.Profile.Edit.Post
import API.Dashboard.Shows.Get.Route qualified as Dashboard.Shows.Get
import API.Dashboard.Shows.New.Get.Route qualified as Dashboard.Shows.New.Get
import API.Dashboard.Shows.New.Post.Route qualified as Dashboard.Shows.New.Post
import API.Dashboard.Shows.Slug.Edit.Get.Route qualified as Dashboard.Shows.Slug.Edit.Get
import API.Dashboard.Shows.Slug.Edit.Post.Route qualified as Dashboard.Shows.Slug.Edit.Post
import API.Dashboard.Shows.Slug.Episode.New.Get.Route qualified as Dashboard.Shows.Slug.Episode.New.Get
import API.Dashboard.Shows.Slug.Episode.New.Post.Route qualified as Dashboard.Shows.Slug.Episode.New.Post
import API.Dashboard.Shows.Slug.Get.Route qualified as Dashboard.Shows.Slug.Get
import API.Dashboard.StationBlog.Get.Route qualified as Dashboard.StationBlog.Get
import API.Dashboard.StationBlog.New.Get.Route qualified as Dashboard.StationBlog.New.Get
import API.Dashboard.StationBlog.New.Post.Route qualified as Dashboard.StationBlog.New.Post
import API.Dashboard.StationBlog.Slug.Delete.Route qualified as Dashboard.StationBlog.Slug.Delete
import API.Dashboard.StationBlog.Slug.Edit.Get.Route qualified as Dashboard.StationBlog.Slug.Edit.Get
import API.Dashboard.StationBlog.Slug.Edit.Post.Route qualified as Dashboard.StationBlog.Slug.Edit.Post
import API.Dashboard.StationBlog.Slug.Get.Route qualified as Dashboard.StationBlog.Slug.Get
import API.Dashboard.Users.Delete.Route qualified as Dashboard.Users.Delete
import API.Dashboard.Users.Detail.Get.Route qualified as Dashboard.Users.Detail.Get
import API.Dashboard.Users.Edit.Get.Route qualified as Dashboard.Users.Edit.Get
import API.Dashboard.Users.Edit.Post.Route qualified as Dashboard.Users.Edit.Post
import API.Dashboard.Users.Get.Route qualified as Dashboard.Users.Get
import API.Dashboard.Users.Role.Patch.Route qualified as Dashboard.Users.Role.Patch
import API.Dashboard.Users.Suspend.Post.Route qualified as Dashboard.Users.Suspend.Post
import API.Dashboard.Users.Unsuspend.Post.Route qualified as Dashboard.Users.Unsuspend.Post
import API.Donate.Get.Route qualified as Donate.Get
import API.Events.Event.Get.Route qualified as Events.Event.Get
import API.Events.Get.Route qualified as Events.Get
import API.Get.Route qualified as Root.Get
import API.Media.Get.Route qualified as Media.Get
import API.PrivacyPolicy.Get.Route qualified as PrivacyPolicy.Get
import API.Schedule.Get.Route qualified as Schedule
import API.Shows.Get.Route qualified as Shows.Get
import API.Shows.Slug.Blog.Delete.Route qualified as Show.Blog.Delete
import API.Shows.Slug.Blog.Edit.Get.Route qualified as Show.Blog.Edit.Get
import API.Shows.Slug.Blog.Edit.Post.Route qualified as Show.Blog.Edit.Post
import API.Shows.Slug.Blog.Get.Route qualified as Show.Blog.Get
import API.Shows.Slug.Blog.New.Get.Route qualified as Show.Blog.New.Get
import API.Shows.Slug.Blog.New.Post.Route qualified as Show.Blog.New.Post
import API.Shows.Slug.Blog.Post.Get.Route qualified as Show.Blog.Post.Get
import API.Shows.Slug.Episode.Delete.Route qualified as Episodes.Delete
import API.Shows.Slug.Episode.DiscardDraft.Route qualified as Episodes.DiscardDraft
import API.Shows.Slug.Episode.Get.Route qualified as Episodes.Get
import API.Shows.Slug.Episode.Publish.Post.Route qualified as Episodes.Publish.Post
import API.Shows.Slug.Get.Route qualified as Show.Get
import API.Static.Get.Route qualified as Static.Get
import API.TermsOfService.Get.Route qualified as TermsOfService.Get
import API.User.Login.Get.Route qualified as User.Login.Get
import API.User.Login.Post.Route qualified as User.Login.Post
import API.User.Logout.Get.Route qualified as User.Logout.Get
import API.User.Logout.Post.Route qualified as User.Logout.Post
import API.User.Register.Get.Route qualified as User.Register.Get
import API.User.Register.Post.Route qualified as User.Register.Post
import GHC.Generics (Generic)
import Servant (NamedRoutes, (:-))

--------------------------------------------------------------------------------

type API = NamedRoutes Routes

-- | Top-level API routes for KPBJ 95.9FM website.
--
-- Includes standalone public pages and nested route groups for blog, events,
-- shows, user authentication, and the admin dashboard.
data Routes mode = Routes
  { -- | @GET /@ - Home page
    rootGet :: mode :- Root.Get.Route,
    -- | @GET /static@ - Static file serving
    staticGet :: mode :- Static.Get.Route,
    -- | @GET /media@ - Media file serving
    mediaGet :: mode :- Media.Get.Route,
    -- | @GET /about@ - About page
    aboutGet :: mode :- About.Get.Route,
    -- | @GET /donate@ - Donation page
    donateGet :: mode :- Donate.Get.Route,
    -- | @GET /privacy-policy@ - Privacy policy page
    privacyPolicyGet :: mode :- PrivacyPolicy.Get.Route,
    -- | @GET /terms-of-service@ - Terms of service page
    termsOfServiceGet :: mode :- TermsOfService.Get.Route,
    -- | @/blog/...@ - Blog routes
    blog :: mode :- NamedRoutes BlogRoutes,
    -- | @/events/...@ - Events routes
    events :: mode :- NamedRoutes EventsRoutes,
    -- | @/schedule/...@ - Schedule Route
    schedule :: mode :- Schedule.Route,
    -- | @/shows/...@ - Shows routes
    shows :: mode :- NamedRoutes ShowsRoutes,
    -- | @/user/...@ - User authentication routes
    user :: mode :- NamedRoutes UserRoutes,
    -- | @/dashboard/...@ - Admin dashboard routes
    dashboard :: mode :- NamedRoutes DashboardRoutes
  }
  deriving stock (Generic)

-- | Station blog routes under @/blog@.
--
-- Provides listing and detail views for official KPBJ station blog posts.
data BlogRoutes mode = BlogRoutes
  { -- | @GET /blog@ - Blog listing with pagination and search
    list :: mode :- Blog.Get.Route,
    -- | @GET /blog/:id/:slug@ - Blog post detail (canonical URL with slug)
    postWithSlug :: mode :- Blog.Post.Get.RouteWithSlug,
    -- | @GET /blog/:id@ - Blog post detail (redirects to canonical URL)
    postWithoutSlug :: mode :- Blog.Post.Get.RouteWithoutSlug
  }
  deriving stock (Generic)

-- | Community events routes under @/events@.
--
-- Provides calendar views and detail pages for community events.
data EventsRoutes mode = EventsRoutes
  { -- | @GET /events@ - Events calendar with week/month/list views
    list :: mode :- Events.Get.Route,
    -- | @GET /events/:id/:slug@ - Event detail (canonical URL with slug)
    detailWithSlug :: mode :- Events.Event.Get.RouteWithSlug,
    -- | @GET /events/:id@ - Event detail (redirects to canonical URL)
    detailWithoutSlug :: mode :- Events.Event.Get.RouteWithoutSlug
  }
  deriving stock (Generic)

-- | Radio shows routes under @/shows@.
--
-- Includes show listings, individual show pages, and nested routes
-- for show-specific blog posts and episodes.
data ShowsRoutes mode = ShowsRoutes
  { -- | @GET /shows@ - Shows listing with tag and status filtering
    list :: mode :- Shows.Get.Route,
    -- | @GET /shows/:slug@ - Individual show page
    detail :: mode :- Show.Get.Route,
    -- | @/shows/:slug/blog/...@ - Show-specific blog routes
    blog :: mode :- NamedRoutes ShowBlogRoutes,
    -- | @/shows/:slug/episodes/...@ - Show episode routes
    episodes :: mode :- NamedRoutes ShowEpisodesRoutes
  }
  deriving stock (Generic)

-- | Show-specific blog routes under @/shows/:showSlug/blog@.
--
-- Allows hosts to create and manage blog posts for their shows.
data ShowBlogRoutes mode = ShowBlogRoutes
  { -- | @GET /shows/:showSlug/blog@ - Show blog listing
    list :: mode :- Show.Blog.Get.Route,
    -- | @GET /shows/:showSlug/blog/new@ - New blog post form
    newGet :: mode :- Show.Blog.New.Get.Route,
    -- | @POST /shows/:showSlug/blog/new@ - Create new blog post
    newPost :: mode :- Show.Blog.New.Post.Route,
    -- | @GET /shows/:showSlug/blog/:id/:slug@ - Blog post detail (canonical)
    postWithSlug :: mode :- Show.Blog.Post.Get.RouteWithSlug,
    -- | @GET /shows/:showSlug/blog/:id@ - Blog post detail (redirects)
    postWithoutSlug :: mode :- Show.Blog.Post.Get.RouteWithoutSlug,
    -- | @GET /shows/:showSlug/blog/:id/edit@ - Edit blog post form
    editGet :: mode :- Show.Blog.Edit.Get.Route,
    -- | @POST /shows/:showSlug/blog/:id/edit@ - Update blog post
    editPost :: mode :- Show.Blog.Edit.Post.Route,
    -- | @DELETE /shows/:showSlug/blog/:id@ - Delete blog post
    delete :: mode :- Show.Blog.Delete.Route
  }
  deriving stock (Generic)

-- | Show episode routes under @/shows/:showSlug/episodes@.
--
-- Public view and management routes for show episodes.
-- Note: Episode creation routes are under Dashboard.
data ShowEpisodesRoutes mode = ShowEpisodesRoutes
  { -- | @GET /shows/:showSlug/episodes/:episodeNumber@ - Episode detail
    detail :: mode :- Episodes.Get.Route,
    -- | @DELETE /shows/:showSlug/episodes/:episodeNumber@ - Delete episode
    delete :: mode :- Episodes.Delete.Route,
    -- | @DELETE /shows/:showSlug/episodes/:episodeNumber/draft@ - Discard draft episode
    discardDraft :: mode :- Episodes.DiscardDraft.Route,
    -- | @POST /shows/:showSlug/episodes/:episodeNumber/publish@ - Publish draft episode
    publish :: mode :- Episodes.Publish.Post.Route
  }
  deriving stock (Generic)

-- | User authentication routes under @/user@.
--
-- Handles login, logout, and registration flows.
data UserRoutes mode = UserRoutes
  { -- | @GET /user/login@ - Login page
    loginGet :: mode :- User.Login.Get.Route,
    -- | @POST /user/login@ - Process login credentials
    loginPost :: mode :- User.Login.Post.Route,
    -- | @GET /user/logout@ - Logout confirmation page
    logoutGet :: mode :- User.Logout.Get.Route,
    -- | @POST /user/logout@ - Process logout
    logoutPost :: mode :- User.Logout.Post.Route,
    -- | @GET /user/register@ - Registration page
    registerGet :: mode :- User.Register.Get.Route,
    -- | @POST /user/register@ - Process registration
    registerPost :: mode :- User.Register.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard routes under @/dashboard@.
--
-- Provides management interfaces for episodes, blogs, events, shows, and users.
-- Access is restricted based on user roles.
data DashboardRoutes mode = DashboardRoutes
  { -- | @GET /dashboard@ - Dashboard home with stats and recent activity
    home :: mode :- Dashboard.Get.Route,
    -- | @GET /dashboard/episodes@ - Redirect to episodes list
    episodesRedirect :: mode :- Dashboard.Episodes.Redirect.Route,
    -- | @GET /dashboard/profile/edit@ - Edit own profile form
    profileEditGet :: mode :- Dashboard.Profile.Edit.Get.Route,
    -- | @POST /dashboard/profile/edit@ - Update own profile
    profileEditPost :: mode :- Dashboard.Profile.Edit.Post.Route,
    -- | Host-accessible dashboard routes (episodes, blogs, events)
    host :: mode :- NamedRoutes DashboardHostRoutes,
    -- | Admin-only dashboard routes (station blog, shows, users)
    admin :: mode :- NamedRoutes DashboardAdminRoutes
  }
  deriving stock (Generic)

-- | Host-accessible dashboard routes.
--
-- Routes for hosts to manage their show's episodes and blog posts.
data DashboardHostRoutes mode = DashboardHostRoutes
  { -- | @/dashboard/episodes/...@ - Episode management routes
    episodes :: mode :- NamedRoutes DashboardEpisodesRoutes,
    -- | @/dashboard/blogs/...@ - Show blog management routes
    blogs :: mode :- NamedRoutes DashboardBlogsRoutes
  }
  deriving stock (Generic)

-- | Admin-only dashboard routes.
--
-- Routes for admins to manage station blog, shows, events, and users.
data DashboardAdminRoutes mode = DashboardAdminRoutes
  { -- | @/dashboard/station-blog/...@ - Station blog management routes
    stationBlog :: mode :- NamedRoutes DashboardStationBlogRoutes,
    -- | @/dashboard/shows/...@ - Show management routes
    shows :: mode :- NamedRoutes DashboardShowsRoutes,
    -- | @/dashboard/events/...@ - Event management routes
    events :: mode :- NamedRoutes DashboardEventsRoutes,
    -- | @/dashboard/users/...@ - User management routes
    users :: mode :- NamedRoutes DashboardUsersRoutes
  }
  deriving stock (Generic)

-- | Dashboard episode management routes under @/dashboard/episodes@.
data DashboardEpisodesRoutes mode = DashboardEpisodesRoutes
  { -- | @GET /dashboard/episodes/:showSlug@ - Episode list for a show
    list :: mode :- Dashboard.Episodes.Get.Route,
    -- | @GET /dashboard/episodes/:showSlug/:episodeNumber@ - Episode detail
    detail :: mode :- Dashboard.Episodes.Slug.Get.Route,
    -- | @GET /dashboard/episodes/:showSlug/:episodeNumber/edit@ - Edit episode form
    editGet :: mode :- Dashboard.Episodes.Slug.Edit.Get.Route,
    -- | @POST /dashboard/episodes/:showSlug/:episodeNumber/edit@ - Update episode
    editPost :: mode :- Dashboard.Episodes.Slug.Edit.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard show blog management routes under @/dashboard/blogs@.
data DashboardBlogsRoutes mode = DashboardBlogsRoutes
  { -- | @GET /dashboard/blogs/:showSlug@ - Blog post list for a show
    list :: mode :- Dashboard.Blogs.Get.Route,
    -- | @GET /dashboard/blogs/:showSlug/:postSlug@ - Blog post detail
    detail :: mode :- Dashboard.Blogs.Slug.Get.Route
  }
  deriving stock (Generic)

-- | Dashboard event management routes under @/dashboard/events@.
data DashboardEventsRoutes mode = DashboardEventsRoutes
  { -- | @GET /dashboard/events@ - Event list
    list :: mode :- Dashboard.Events.Get.Route,
    -- | @GET /dashboard/events/new@ - New event form
    newGet :: mode :- Dashboard.Events.New.Get.Route,
    -- | @POST /dashboard/events/new@ - Create event
    newPost :: mode :- Dashboard.Events.New.Post.Route,
    -- | @GET /dashboard/events/:slug@ - Event detail
    detail :: mode :- Dashboard.Events.Slug.Get.Route,
    -- | @GET /dashboard/events/:slug/edit@ - Edit event form
    editGet :: mode :- Dashboard.Events.Slug.Edit.Get.Route,
    -- | @POST /dashboard/events/:slug/edit@ - Update event
    editPost :: mode :- Dashboard.Events.Slug.Edit.Post.Route,
    -- | @DELETE /dashboard/events/:slug@ - Delete event
    delete :: mode :- Dashboard.Events.Slug.Delete.Route
  }
  deriving stock (Generic)

-- | Dashboard station blog management routes under @/dashboard/station-blog@.
--
-- For staff and admins to manage official station blog posts.
data DashboardStationBlogRoutes mode = DashboardStationBlogRoutes
  { -- | @GET /dashboard/station-blog@ - Station blog post list
    list :: mode :- Dashboard.StationBlog.Get.Route,
    -- | @GET /dashboard/station-blog/new@ - New station blog post form
    newGet :: mode :- Dashboard.StationBlog.New.Get.Route,
    -- | @POST /dashboard/station-blog/new@ - Create station blog post
    newPost :: mode :- Dashboard.StationBlog.New.Post.Route,
    -- | @GET /dashboard/station-blog/:slug@ - Station blog post detail
    detail :: mode :- Dashboard.StationBlog.Slug.Get.Route,
    -- | @GET /dashboard/station-blog/:slug/edit@ - Edit station blog post form
    editGet :: mode :- Dashboard.StationBlog.Slug.Edit.Get.Route,
    -- | @POST /dashboard/station-blog/:slug/edit@ - Update station blog post
    editPost :: mode :- Dashboard.StationBlog.Slug.Edit.Post.Route,
    -- | @DELETE /dashboard/station-blog/:slug@ - Delete station blog post
    delete :: mode :- Dashboard.StationBlog.Slug.Delete.Route
  }
  deriving stock (Generic)

-- | Dashboard show management routes under @/dashboard/shows@.
--
-- For admins to create and manage radio shows, and hosts to create episodes.
data DashboardShowsRoutes mode = DashboardShowsRoutes
  { -- | @GET /dashboard/shows@ - Show list
    list :: mode :- Dashboard.Shows.Get.Route,
    -- | @GET /dashboard/shows/new@ - New show form
    newGet :: mode :- Dashboard.Shows.New.Get.Route,
    -- | @POST /dashboard/shows/new@ - Create show
    newPost :: mode :- Dashboard.Shows.New.Post.Route,
    -- | @GET /dashboard/shows/:slug@ - Show detail
    detail :: mode :- Dashboard.Shows.Slug.Get.Route,
    -- | @GET /dashboard/shows/:slug/edit@ - Edit show form
    editGet :: mode :- Dashboard.Shows.Slug.Edit.Get.Route,
    -- | @POST /dashboard/shows/:slug/edit@ - Update show
    editPost :: mode :- Dashboard.Shows.Slug.Edit.Post.Route,
    -- | @GET /dashboard/shows/:slug/episodes/new@ - New episode upload form
    episodeNewGet :: mode :- Dashboard.Shows.Slug.Episode.New.Get.Route,
    -- | @POST /dashboard/shows/:slug/episodes/new@ - Create new episode
    episodeNewPost :: mode :- Dashboard.Shows.Slug.Episode.New.Post.Route
  }
  deriving stock (Generic)

-- | Dashboard user management routes under @/dashboard/users@.
--
-- Admin-only routes for managing user accounts, roles, and suspensions.
data DashboardUsersRoutes mode = DashboardUsersRoutes
  { -- | @GET /dashboard/users@ - User list with pagination and search
    list :: mode :- Dashboard.Users.Get.Route,
    -- | @GET /dashboard/users/:id@ - User detail
    detail :: mode :- Dashboard.Users.Detail.Get.Route,
    -- | @GET /dashboard/users/:id/edit@ - Edit user form
    editGet :: mode :- Dashboard.Users.Edit.Get.Route,
    -- | @POST /dashboard/users/:id/edit@ - Update user
    editPost :: mode :- Dashboard.Users.Edit.Post.Route,
    -- | @PATCH /dashboard/users/:id/role@ - Update user role
    rolePatch :: mode :- Dashboard.Users.Role.Patch.Route,
    -- | @POST /dashboard/users/:id/suspend@ - Suspend user
    suspendPost :: mode :- Dashboard.Users.Suspend.Post.Route,
    -- | @POST /dashboard/users/:id/unsuspend@ - Unsuspend user
    unsuspendPost :: mode :- Dashboard.Users.Unsuspend.Post.Route,
    -- | @DELETE /dashboard/users/:id@ - Delete user
    delete :: mode :- Dashboard.Users.Delete.Route
  }
  deriving stock (Generic)
