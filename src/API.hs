module API where

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Archive.Get qualified as Archive.Get
import API.Blog.Delete qualified as Blog.Delete
import API.Blog.Edit.Get qualified as Blog.Edit.Get
import API.Blog.Edit.Post qualified as Blog.Edit.Post
import API.Blog.Get qualified as Blog.Get
import API.Blog.New.Get qualified as Blog.New.Get
import API.Blog.New.Post qualified as Blog.New.Post
import API.Blog.Post.Get qualified as Blog.Post.Get
import API.Dashboard.Blogs.Get qualified as Dashboard.Blogs.Get
import API.Dashboard.Blogs.Slug.Get qualified as Dashboard.Blogs.Slug.Get
import API.Dashboard.Episodes.Get qualified as Dashboard.Episodes.Get
import API.Dashboard.Episodes.Slug.Edit.Get qualified as Dashboard.Episodes.Slug.Edit.Get
import API.Dashboard.Episodes.Slug.Edit.Post qualified as Dashboard.Episodes.Slug.Edit.Post
import API.Dashboard.Episodes.Slug.Get qualified as Dashboard.Episodes.Slug.Get
import API.Dashboard.Get qualified as Dashboard.Get
import API.Dashboard.Shows.Get qualified as Dashboard.Shows.Get
import API.Dashboard.Shows.New.Get qualified as Dashboard.Shows.New.Get
import API.Dashboard.Shows.New.Post qualified as Dashboard.Shows.New.Post
import API.Dashboard.Shows.Slug.Edit.Get qualified as Dashboard.Shows.Slug.Edit.Get
import API.Dashboard.Shows.Slug.Edit.Post qualified as Dashboard.Shows.Slug.Edit.Post
import API.Dashboard.Shows.Slug.Get qualified as Dashboard.Shows.Slug.Get
import API.Dashboard.Users.Delete qualified as Dashboard.Users.Delete
import API.Dashboard.Users.Detail.Get qualified as Dashboard.Users.Detail.Get
import API.Dashboard.Users.Edit.Get qualified as Dashboard.Users.Edit.Get
import API.Dashboard.Users.Edit.Post qualified as Dashboard.Users.Edit.Post
import API.Dashboard.Users.Get qualified as Dashboard.Users.Get
import API.Dashboard.Users.Role.Patch qualified as Dashboard.Users.Role.Patch
import API.Dashboard.Users.Suspend.Post qualified as Dashboard.Users.Suspend.Post
import API.Dashboard.Users.Unsuspend.Post qualified as Dashboard.Users.Unsuspend.Post
import API.Donate.Get qualified as Donate.Get
import API.Events.Delete qualified as Events.Delete
import API.Events.Edit.Get qualified as Events.Edit.Get
import API.Events.Edit.Post qualified as Events.Edit.Post
import API.Events.Event.Get qualified as Events.Event.Get
import API.Events.Get qualified as Events.Get
import API.Events.New.Get qualified as Events.New.Get
import API.Events.New.Post qualified as Events.New.Post
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
import Hasql.Pool qualified as HSQL.Pool
import Log (MonadLog)
import OpenTelemetry.Trace (Tracer)
import Servant ((:<|>) (..))
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

runApi :: IO ()
runApi = App.runApp @API server ()

--------------------------------------------------------------------------------

type API =
  Root.Get.Route
    :<|> Static.Get.Route
    :<|> Media.Get.Route
    :<|> About.Get.Route
    :<|> Archive.Get.Route
    :<|> Blog.Get.Route
    :<|> Blog.New.Get.Route
    :<|> Blog.New.Post.Route
    :<|> Blog.Post.Get.RouteWithSlug
    :<|> Blog.Post.Get.RouteWithoutSlug
    :<|> Blog.Edit.Get.Route
    :<|> Blog.Edit.Post.Route
    :<|> Blog.Delete.Route
    :<|> Donate.Get.Route
    :<|> Episodes.New.Get.Route
    :<|> Episodes.New.Post.Route
    :<|> Events.Get.Route
    :<|> Events.New.Get.Route
    :<|> Events.New.Post.Route
    :<|> Events.Event.Get.RouteWithSlug
    :<|> Events.Event.Get.RouteWithoutSlug
    :<|> Events.Edit.Get.Route
    :<|> Events.Edit.Post.Route
    :<|> Events.Delete.Route
    :<|> Dashboard.Get.Route
    :<|> Dashboard.Episodes.Get.Route
    :<|> Dashboard.Episodes.Slug.Get.Route
    :<|> Dashboard.Episodes.Slug.Edit.Get.Route
    :<|> Dashboard.Episodes.Slug.Edit.Post.Route
    :<|> Dashboard.Blogs.Get.Route
    :<|> Dashboard.Blogs.Slug.Get.Route
    :<|> Dashboard.Users.Get.Route
    :<|> Dashboard.Shows.Get.Route
    :<|> PrivacyPolicy.Get.Route
    :<|> TermsOfService.Get.Route
    :<|> Shows.Get.Route
    :<|> Shows.Schedule.Get.Route
    :<|> Show.Get.Route
    :<|> Show.Blog.Get.Route
    :<|> Show.Blog.New.Get.Route
    :<|> Show.Blog.New.Post.Route
    :<|> Show.Blog.Post.Get.RouteWithSlug
    :<|> Show.Blog.Post.Get.RouteWithoutSlug
    :<|> Show.Blog.Edit.Get.Route
    :<|> Show.Blog.Edit.Post.Route
    :<|> Show.Blog.Delete.Route
    :<|> Dashboard.Shows.Slug.Edit.Get.Route
    :<|> Dashboard.Shows.Slug.Edit.Post.Route
    :<|> Dashboard.Shows.Slug.Get.Route
    :<|> Episodes.Get.RouteWithSlug
    :<|> Episodes.Get.RouteWithoutSlug
    :<|> Episodes.Delete.Route
    :<|> Episodes.DiscardDraft.Route
    :<|> Episodes.Publish.Post.Route
    :<|> Dashboard.Shows.New.Get.Route
    :<|> Dashboard.Shows.New.Post.Route
    :<|> Dashboard.Users.Detail.Get.Route
    :<|> Dashboard.Users.Edit.Get.Route
    :<|> Dashboard.Users.Edit.Post.Route
    :<|> Dashboard.Users.Role.Patch.Route
    :<|> Dashboard.Users.Suspend.Post.Route
    :<|> Dashboard.Users.Unsuspend.Post.Route
    :<|> Dashboard.Users.Delete.Route
    :<|> User.Login.Get.Route
    :<|> User.Login.Post.Route
    :<|> User.Logout.Get.Route
    :<|> User.Logout.Post.Route
    :<|> User.Register.Get.Route
    :<|> User.Register.Post.Route

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
  Root.Get.handler
    :<|> Static.Get.handler env
    :<|> Media.Get.handler
    :<|> About.Get.handler
    :<|> Archive.Get.handler
    :<|> Blog.Get.handler
    :<|> Blog.New.Get.handler
    :<|> Blog.New.Post.handler
    :<|> Blog.Post.Get.handlerWithSlug
    :<|> Blog.Post.Get.handlerWithoutSlug
    :<|> Blog.Edit.Get.handler
    :<|> Blog.Edit.Post.handler
    :<|> Blog.Delete.handler
    :<|> Donate.Get.handler
    :<|> Episodes.New.Get.handler
    :<|> Episodes.New.Post.handler
    :<|> Events.Get.handler
    :<|> Events.New.Get.handler
    :<|> Events.New.Post.handler
    :<|> Events.Event.Get.handlerWithSlug
    :<|> Events.Event.Get.handlerWithoutSlug
    :<|> Events.Edit.Get.handler
    :<|> Events.Edit.Post.handler
    :<|> Events.Delete.handler
    :<|> Dashboard.Get.handler
    :<|> Dashboard.Episodes.Get.handler
    :<|> Dashboard.Episodes.Slug.Get.handler
    :<|> Dashboard.Episodes.Slug.Edit.Get.handler
    :<|> Dashboard.Episodes.Slug.Edit.Post.handler
    :<|> Dashboard.Blogs.Get.handler
    :<|> Dashboard.Blogs.Slug.Get.handler
    :<|> Dashboard.Users.Get.handler
    :<|> Dashboard.Shows.Get.handler
    :<|> PrivacyPolicy.Get.handler
    :<|> TermsOfService.Get.handler
    :<|> Shows.Get.handler
    :<|> Shows.Schedule.Get.handler
    :<|> Show.Get.handler
    :<|> Show.Blog.Get.handler
    :<|> Show.Blog.New.Get.handler
    :<|> Show.Blog.New.Post.handler
    :<|> Show.Blog.Post.Get.handlerWithSlug
    :<|> Show.Blog.Post.Get.handlerWithoutSlug
    :<|> Show.Blog.Edit.Get.handler
    :<|> Show.Blog.Edit.Post.handler
    :<|> Show.Blog.Delete.handler
    :<|> Dashboard.Shows.Slug.Edit.Get.handler
    :<|> Dashboard.Shows.Slug.Edit.Post.handler
    :<|> Dashboard.Shows.Slug.Get.handler
    :<|> Episodes.Get.handlerWithSlug
    :<|> Episodes.Get.handlerWithoutSlug
    :<|> Episodes.Delete.handler
    :<|> Episodes.DiscardDraft.handler
    :<|> Episodes.Publish.Post.handler
    :<|> Dashboard.Shows.New.Get.handler
    :<|> Dashboard.Shows.New.Post.handler
    :<|> Dashboard.Users.Detail.Get.handler
    :<|> Dashboard.Users.Edit.Get.handler
    :<|> Dashboard.Users.Edit.Post.handler
    :<|> Dashboard.Users.Role.Patch.handler
    :<|> Dashboard.Users.Suspend.Post.handler
    :<|> Dashboard.Users.Unsuspend.Post.handler
    :<|> Dashboard.Users.Delete.handler
    :<|> User.Login.Get.handler
    :<|> User.Login.Post.handler
    :<|> User.Logout.Get.handler
    :<|> User.Logout.Post.handler
    :<|> User.Register.Get.handler
    :<|> User.Register.Post.handler

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

-- | Route: GET /blog/new
blogNewGetLink :: Links.Link
blogNewGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Get.Route)

-- | Route: POST /blog/new
blogNewPostLink :: Links.Link
blogNewPostLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Post.Route)

-- | Route: GET /blog/:id/:slug
blogPostGetLink :: BlogPosts.Id -> Slug -> Links.Link
blogPostGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Post.Get.RouteWithSlug)

-- | Route: GET /blog/:id (without slug, redirects to canonical)
blogPostGetLinkById :: BlogPosts.Id -> Links.Link
blogPostGetLinkById = Links.safeLink (Proxy @API) (Proxy @Blog.Post.Get.RouteWithoutSlug)

-- | Route: GET /blog/:id/:slug/edit
blogEditGetLink :: BlogPosts.Id -> Slug -> Links.Link
blogEditGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Edit.Get.Route)

-- | Route: POST /blog/:id/:slug/edit
blogEditPostLink :: BlogPosts.Id -> Slug -> Links.Link
blogEditPostLink = Links.safeLink (Proxy @API) (Proxy @Blog.Edit.Post.Route)

-- | Route: DELETE /blog/:post_id/:post_slug
blogDeleteLink :: BlogPosts.Id -> Slug -> Links.Link
blogDeleteLink = Links.safeLink (Proxy @API) (Proxy @Blog.Delete.Route)

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

-- | Route: GET /events/new
eventsNewGetLink :: Links.Link
eventsNewGetLink = Links.safeLink (Proxy @API) (Proxy @Events.New.Get.Route)

-- | Route: POST /events/new
eventsNewPostLink :: Links.Link
eventsNewPostLink = Links.safeLink (Proxy @API) (Proxy @Events.New.Post.Route)

-- | Route: GET /events/:id/:slug
eventGetLink :: Events.Id -> Slug -> Links.Link
eventGetLink = Links.safeLink (Proxy @API) (Proxy @Events.Event.Get.RouteWithSlug)

-- | Route: GET /events/:id (without slug, redirects to canonical)
eventGetLinkById :: Events.Id -> Links.Link
eventGetLinkById = Links.safeLink (Proxy @API) (Proxy @Events.Event.Get.RouteWithoutSlug)

-- | Route: GET /events/:id/:slug/edit
eventEditGetLink :: Events.Id -> Slug -> Links.Link
eventEditGetLink = Links.safeLink (Proxy @API) (Proxy @Events.Edit.Get.Route)

-- | Route: POST /events/:id/:slug/edit
eventEditPostLink :: Events.Id -> Slug -> Links.Link
eventEditPostLink = Links.safeLink (Proxy @API) (Proxy @Events.Edit.Post.Route)

-- | Route: DELETE /events/:event_id/:event_slug
eventDeleteLink :: Events.Id -> Slug -> Links.Link
eventDeleteLink = Links.safeLink (Proxy @API) (Proxy @Events.Delete.Route)

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
