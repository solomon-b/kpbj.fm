module API where

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Admin.Shows.Get qualified as Admin.Shows.Get
import API.Admin.Shows.New.Get qualified as Admin.Shows.New.Get
import API.Admin.Shows.New.Post qualified as Admin.Shows.New.Post
import API.Admin.Users.Delete qualified as Admin.Users.Delete
import API.Admin.Users.Detail.Get qualified as Admin.Users.Detail.Get
import API.Admin.Users.Edit.Get qualified as Admin.Users.Edit.Get
import API.Admin.Users.Edit.Post qualified as Admin.Users.Edit.Post
import API.Admin.Users.Get qualified as Admin.Users.Get
import API.Admin.Users.Role.Patch qualified as Admin.Users.Role.Patch
import API.Admin.Users.Suspend.Post qualified as Admin.Users.Suspend.Post
import API.Admin.Users.Unsuspend.Post qualified as Admin.Users.Unsuspend.Post
import API.Archive.Get qualified as Archive.Get
import API.Blog.Delete qualified as Blog.Delete
import API.Blog.Edit.Get qualified as Blog.Edit.Get
import API.Blog.Edit.Post qualified as Blog.Edit.Post
import API.Blog.Get qualified as Blog.Get
import API.Blog.New.Get qualified as Blog.New.Get
import API.Blog.New.Post qualified as Blog.New.Post
import API.Blog.Post.Get qualified as Blog.Post.Get
import API.Dashboard.Blog.Get qualified as Dashboard.Blog.Get
import API.Dashboard.Episodes.Episode.Get qualified as Dashboard.Episodes.Episode.Get
import API.Dashboard.Episodes.Get qualified as Dashboard.Episodes.Get
import API.Dashboard.Get qualified as Dashboard.Get
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
import API.Shows.Slug.Edit.Get qualified as Show.Edit.Get
import API.Shows.Slug.Edit.Post qualified as Show.Edit.Post
import API.Shows.Slug.Episode.Delete qualified as Episodes.Delete
import API.Shows.Slug.Episode.DiscardDraft qualified as Episodes.DiscardDraft
import API.Shows.Slug.Episode.Edit.Get qualified as Episodes.Edit.Get
import API.Shows.Slug.Episode.Edit.Post qualified as Episodes.Edit.Post
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
    :<|> Episodes.Edit.Get.Route
    :<|> Episodes.Edit.Post.Route
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
    :<|> Dashboard.Episodes.Episode.Get.Route
    :<|> Dashboard.Blog.Get.Route
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
    :<|> Show.Edit.Get.Route
    :<|> Show.Edit.Post.Route
    :<|> Episodes.Get.RouteWithSlug
    :<|> Episodes.Get.RouteWithoutSlug
    :<|> Episodes.Delete.Route
    :<|> Episodes.DiscardDraft.Route
    :<|> Episodes.Publish.Post.Route
    :<|> Admin.Shows.Get.Route
    :<|> Admin.Shows.New.Get.Route
    :<|> Admin.Shows.New.Post.Route
    :<|> Admin.Users.Get.Route
    :<|> Admin.Users.Detail.Get.Route
    :<|> Admin.Users.Edit.Get.Route
    :<|> Admin.Users.Edit.Post.Route
    :<|> Admin.Users.Role.Patch.Route
    :<|> Admin.Users.Suspend.Post.Route
    :<|> Admin.Users.Unsuspend.Post.Route
    :<|> Admin.Users.Delete.Route
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
    :<|> Episodes.Edit.Get.handler
    :<|> Episodes.Edit.Post.handler
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
    :<|> Dashboard.Episodes.Episode.Get.handler
    :<|> Dashboard.Blog.Get.handler
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
    :<|> Show.Edit.Get.handler
    :<|> Show.Edit.Post.handler
    :<|> Episodes.Get.handlerWithSlug
    :<|> Episodes.Get.handlerWithoutSlug
    :<|> Episodes.Delete.handler
    :<|> Episodes.DiscardDraft.handler
    :<|> Episodes.Publish.Post.handler
    :<|> Admin.Shows.Get.handler
    :<|> Admin.Shows.New.Get.handler
    :<|> Admin.Shows.New.Post.handler
    :<|> Admin.Users.Get.handler
    :<|> Admin.Users.Detail.Get.handler
    :<|> Admin.Users.Edit.Get.handler
    :<|> Admin.Users.Edit.Post.handler
    :<|> Admin.Users.Role.Patch.handler
    :<|> Admin.Users.Suspend.Post.handler
    :<|> Admin.Users.Unsuspend.Post.handler
    :<|> Admin.Users.Delete.handler
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

-- | Route: GET /admin/shows
adminShowsGetLink :: Maybe Int64 -> Maybe (Filter Text) -> Maybe (Filter Shows.Status) -> Links.Link
adminShowsGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Shows.Get.Route)

-- | Route: GET /admin/shows/new
adminShowsNewGetLink :: Links.Link
adminShowsNewGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Shows.New.Get.Route)

-- | Route: POST /admin/shows/new
adminShowsNewPostLink :: Links.Link
adminShowsNewPostLink = Links.safeLink (Proxy @API) (Proxy @Admin.Shows.New.Post.Route)

-- | Route: GET /admin/users
adminUsersGetLink :: Maybe Int64 -> Maybe (Filter Text) -> Maybe (Filter UserMetadata.UserRole) -> Maybe (Filter UserSortBy) -> Links.Link
adminUsersGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Get.Route)

-- | Route: GET /admin/users/:id
adminUserDetailGetLink :: User.Id -> Links.Link
adminUserDetailGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Detail.Get.Route)

-- | Route: GET /admin/users/:id/edit
adminUserEditGetLink :: User.Id -> Links.Link
adminUserEditGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Edit.Get.Route)

-- | Route: POST /admin/users/:id/edit
adminUserEditPostLink :: User.Id -> Links.Link
adminUserEditPostLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Edit.Post.Route)

-- | Route: PATCH /admin/users/:id/role
adminUserRolePatchLink :: User.Id -> Links.Link
adminUserRolePatchLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Role.Patch.Route)

-- | Route: DELETE /admin/users/:id
adminUserDeleteLink :: User.Id -> Links.Link
adminUserDeleteLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Delete.Route)

-- | Route: POST /admin/users/:id/suspend
adminUserSuspendPostLink :: User.Id -> Links.Link
adminUserSuspendPostLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Suspend.Post.Route)

-- | Route: POST /admin/users/:id/unsuspend
adminUserUnsuspendPostLink :: User.Id -> Links.Link
adminUserUnsuspendPostLink = Links.safeLink (Proxy @API) (Proxy @Admin.Users.Unsuspend.Post.Route)

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

-- | Route: GET /shows/:slug/edit
showEditGetLink :: Slug -> Links.Link
showEditGetLink = Links.safeLink (Proxy @API) (Proxy @Show.Edit.Get.Route)

-- | Route: POST /shows/:slug/edit
showEditPostLink :: Slug -> Links.Link
showEditPostLink = Links.safeLink (Proxy @API) (Proxy @Show.Edit.Post.Route)

-- | Route: GET /shows/:show_slug/episodes/:episode_id/:slug
episodesGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesGetLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Get.RouteWithSlug)

-- | Route: GET /shows/:show_slug/episodes/:episode_id (without slug, redirects to canonical)
episodesGetLinkById :: Slug -> Episodes.Id -> Links.Link
episodesGetLinkById = Links.safeLink (Proxy @API) (Proxy @Episodes.Get.RouteWithoutSlug)

-- | Route: POST /shows/:show_slug/episodes/:episode_id/:slug/edit
episodesEditPostLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesEditPostLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Edit.Post.Route)

-- | Route: GET /shows/:show_slug/episodes/new
episodesNewGetLink :: Slug -> Links.Link
episodesNewGetLink = Links.safeLink (Proxy @API) (Proxy @Episodes.New.Get.Route)

-- | Route: POST /shows/:show_slug/episodes/new
episodesNewPostLink :: Slug -> Links.Link
episodesNewPostLink = Links.safeLink (Proxy @API) (Proxy @Episodes.New.Post.Route)

-- | Route: GET /shows/:show_slug/episodes/:episode_id/:slug/edit
episodesEditGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
episodesEditGetLink = Links.safeLink (Proxy @API) (Proxy @Episodes.Edit.Get.Route)

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
hostDashboardGetLink :: Maybe Slug -> Links.Link
hostDashboardGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Get.Route)

-- | Route: GET /dashboard/episodes
dashboardEpisodesGetLink :: Maybe Slug -> Links.Link
dashboardEpisodesGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Get.Route)

-- | Route: GET /dashboard/episodes/:show_slug/:episode_id/:slug
dashboardEpisodeGetLink :: Slug -> Episodes.Id -> Slug -> Links.Link
dashboardEpisodeGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Episodes.Episode.Get.Route)

-- | Route: GET /dashboard/blog
dashboardBlogGetLink :: Maybe Slug -> Links.Link
dashboardBlogGetLink = Links.safeLink (Proxy @API) (Proxy @Dashboard.Blog.Get.Route)
