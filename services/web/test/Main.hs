module Main where

--------------------------------------------------------------------------------

import API.Blog.Get.HandlerSpec qualified as BlogGetHandler
import API.Blog.Post.Get.HandlerSpec qualified as BlogPostGetHandler
import API.Dashboard.Blogs.Get.HandlerSpec qualified as DashboardBlogsGetHandler
import API.Dashboard.Blogs.New.Get.HandlerSpec qualified as DashboardBlogsNewGetHandler
import API.Dashboard.Blogs.New.Post.HandlerSpec qualified as DashboardBlogsNewPostHandler
import API.Dashboard.Blogs.Slug.Delete.HandlerSpec qualified as DashboardBlogsDeleteHandler
import API.Dashboard.Blogs.Slug.Edit.Get.HandlerSpec qualified as DashboardBlogsEditGetHandler
import API.Dashboard.Blogs.Slug.Edit.Post.HandlerSpec qualified as DashboardBlogsEditPostHandler
import API.Dashboard.Blogs.Slug.Get.HandlerSpec qualified as DashboardBlogsSlugGetHandler
import API.Dashboard.EphemeralUploads.Get.HandlerSpec qualified as DashboardEphemeralUploadsGetHandler
import API.Dashboard.EphemeralUploads.Id.Delete.HandlerSpec qualified as DashboardEphemeralUploadsDeleteHandler
import API.Dashboard.EphemeralUploads.Id.Edit.Get.HandlerSpec qualified as DashboardEphemeralUploadsEditGetHandler
import API.Dashboard.EphemeralUploads.Id.Flag.Post.HandlerSpec qualified as DashboardEphemeralUploadsFlagHandler
import API.Dashboard.EphemeralUploads.Id.Unflag.Post.HandlerSpec qualified as DashboardEphemeralUploadsUnflagHandler
import API.Dashboard.EphemeralUploads.New.Get.HandlerSpec qualified as DashboardEphemeralUploadsNewGetHandler
import API.Dashboard.Episodes.Get.HandlerSpec qualified as DashboardEpisodesGetHandler
import API.Dashboard.Episodes.Redirect.HandlerSpec qualified as DashboardEpisodesRedirectHandler
import API.Dashboard.Episodes.Slug.Delete.HandlerSpec qualified as DashboardEpisodesDeleteHandler
import API.Dashboard.Episodes.Slug.Edit.Get.HandlerSpec qualified as EpisodeEditHandler
import API.Dashboard.Episodes.Slug.Edit.Post.HandlerSpec qualified as DashboardEpisodesEditPostHandler
import API.Dashboard.Episodes.Slug.Get.HandlerSpec qualified as DashboardEpisodesSlugGetHandler
import API.Dashboard.Events.Get.HandlerSpec qualified as DashboardEventsGetHandler
import API.Dashboard.Events.New.Get.HandlerSpec qualified as DashboardEventsNewGetHandler
import API.Dashboard.Events.New.Post.HandlerSpec qualified as DashboardEventsNewPostHandler
import API.Dashboard.Events.Slug.Delete.HandlerSpec qualified as DashboardEventsDeleteHandler
import API.Dashboard.Events.Slug.Edit.Get.HandlerSpec qualified as DashboardEventsEditGetHandler
import API.Dashboard.Events.Slug.Edit.Post.HandlerSpec qualified as DashboardEventsEditPostHandler
import API.Dashboard.Events.Slug.Feature.HandlerSpec qualified as DashboardEventsFeatureHandler
import API.Dashboard.Events.Slug.Get.HandlerSpec qualified as DashboardEventsSlugGetHandler
import API.Dashboard.MissingEpisodes.Get.HandlerSpec qualified as DashboardMissingEpisodesGetHandler
import API.Dashboard.Profile.Edit.Get.HandlerSpec qualified as DashboardProfileEditGetHandler
import API.Dashboard.Profile.Edit.Post.HandlerSpec qualified as DashboardProfileEditPostHandler
import API.Dashboard.Shows.Get.HandlerSpec qualified as DashboardShowsGetHandler
import API.Dashboard.Shows.New.Get.HandlerSpec qualified as DashboardShowsNewGetHandler
import API.Dashboard.Shows.New.Post.HandlerSpec qualified as DashboardShowsNewPostHandler
import API.Dashboard.Shows.Slug.Delete.HandlerSpec qualified as DashboardShowsDeleteHandler
import API.Dashboard.Shows.Slug.Edit.Get.HandlerSpec qualified as DashboardShowsSlugEditGetHandler
import API.Dashboard.Shows.Slug.Edit.Post.HandlerSpec qualified as DashboardShowsEditPostHandler
import API.Dashboard.Shows.Slug.Edit.Post.ScheduleDiffSpec qualified as ScheduleDiff
import API.Dashboard.Shows.Slug.Episode.New.Post.HandlerSpec qualified as DashboardShowsEpisodeNewPostHandler
import API.Dashboard.Shows.Slug.Get.HandlerSpec qualified as DashboardShowsSlugGetHandler
import API.Dashboard.SitePages.Get.HandlerSpec qualified as DashboardSitePagesGetHandler
import API.Dashboard.SitePages.Slug.Edit.Get.HandlerSpec qualified as DashboardSitePagesEditGetHandler
import API.Dashboard.SitePages.Slug.Edit.Post.HandlerSpec qualified as DashboardSitePagesEditPostHandler
import API.Dashboard.SitePages.Slug.History.Get.HandlerSpec qualified as DashboardSitePagesHistoryGetHandler
import API.Dashboard.SitePages.Slug.Revisions.Id.Get.HandlerSpec qualified as DashboardSitePagesRevisionGetHandler
import API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.HandlerSpec qualified as DashboardSitePagesRestorePostHandler
import API.Dashboard.StationBlog.Get.HandlerSpec qualified as DashboardStationBlogGetHandler
import API.Dashboard.StationBlog.New.Get.HandlerSpec qualified as DashboardStationBlogNewGetHandler
import API.Dashboard.StationBlog.New.Post.HandlerSpec qualified as DashboardStationBlogNewPostHandler
import API.Dashboard.StationBlog.Slug.Delete.HandlerSpec qualified as DashboardStationBlogDeleteHandler
import API.Dashboard.StationBlog.Slug.Edit.Get.HandlerSpec qualified as DashboardStationBlogEditGetHandler
import API.Dashboard.StationBlog.Slug.Edit.Post.HandlerSpec qualified as DashboardStationBlogEditPostHandler
import API.Dashboard.StationBlog.Slug.Get.HandlerSpec qualified as DashboardStationBlogSlugGetHandler
import API.Dashboard.StationIds.Get.HandlerSpec qualified as DashboardStationIdsGetHandler
import API.Dashboard.StationIds.Id.Delete.HandlerSpec qualified as DashboardStationIdsDeleteHandler
import API.Dashboard.StationIds.New.Get.HandlerSpec qualified as DashboardStationIdsNewGetHandler
import API.Dashboard.StreamSettings.Episodes.Search.Get.HandlerSpec qualified as DashboardStreamSettingsEpisodeSearchHandler
import API.Dashboard.Users.Delete.HandlerSpec qualified as DashboardUsersDeleteHandler
import API.Dashboard.Users.Detail.Get.HandlerSpec qualified as DashboardUsersDetailGetHandler
import API.Dashboard.Users.Edit.Get.HandlerSpec qualified as DashboardUsersEditGetHandler
import API.Dashboard.Users.Edit.Post.HandlerSpec qualified as DashboardUsersEditPostHandler
import API.Dashboard.Users.Get.HandlerSpec qualified as DashboardUsersGetHandler
import API.Dashboard.Users.Role.Patch.HandlerSpec qualified as DashboardUsersRolePatchHandler
import API.Dashboard.Users.Suspend.Post.HandlerSpec qualified as DashboardUsersSuspendHandler
import API.Dashboard.Users.Unsuspend.Post.HandlerSpec qualified as DashboardUsersUnsuspendHandler
import API.Events.Event.Get.HandlerSpec qualified as EventHandler
import API.Events.Get.HandlerSpec qualified as EventsHandler
import API.Get.HandlerSpec qualified as HomeHandler
import API.Schedule.Get.HandlerSpec qualified as ScheduleHandler
import API.Shows.Get.HandlerSpec qualified as ShowsHandler
import API.Shows.Slug.Blog.Get.HandlerSpec qualified as ShowBlogHandler
import API.Shows.Slug.Blog.Post.Get.HandlerSpec qualified as ShowBlogPostHandler
import API.Shows.Slug.Episode.Get.HandlerSpec qualified as ShowEpisodeHandler
import API.Shows.Slug.Get.HandlerSpec qualified as ShowDetailHandler
import API.User.ForgotPassword.Post.HandlerSpec qualified as UserForgotPasswordHandler
import API.User.Login.Post.HandlerSpec qualified as UserLoginHandler
import API.User.Register.Post.HandlerSpec qualified as UserRegisterHandler
import App.CookieSpec qualified as Cookie
import App.DomainsSpec qualified as Domains
import App.Handler.CombinatorsSpec qualified as Combinators
import Data.Maybe (fromMaybe)
import Domain.Types.FileUploadSpec qualified as FileUpload
import Domain.Types.SlugSpec qualified as Slug
import Domain.Types.StorageBackendSpec qualified as StorageBackend
import Effects.ContentSanitizationSpec qualified as ContentSanitization
import Effects.Database.Tables.BlogPostsSpec qualified as BlogPosts
import Effects.Database.Tables.BlogTagsSpec qualified as BlogTags
import Effects.Database.Tables.CurrentlyAiringSpec qualified as CurrentlyAiring
import Effects.Database.Tables.EmailVerificationTokensSpec qualified as EmailVerificationTokens
import Effects.Database.Tables.EphemeralUploadsSpec qualified as EphemeralUploads
import Effects.Database.Tables.EpisodeTrackSpec qualified as EpisodeTrack
import Effects.Database.Tables.EpisodesSpec qualified as Episodes
import Effects.Database.Tables.EventsSpec qualified as Events
import Effects.Database.Tables.PasswordResetTokensSpec qualified as PasswordResetTokens
import Effects.Database.Tables.PlaybackHistorySpec qualified as PlaybackHistory
import Effects.Database.Tables.ShowBlogPostsSpec qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTagsSpec qualified as ShowBlogTags
import Effects.Database.Tables.ShowHostSpec qualified as ShowHost
import Effects.Database.Tables.ShowScheduleMissingEpisodesSpec qualified as ShowScheduleMissingEpisodes
import Effects.Database.Tables.ShowScheduleSpec qualified as ShowSchedule
import Effects.Database.Tables.ShowTagsSpec qualified as ShowTags
import Effects.Database.Tables.ShowsSpec qualified as Shows
import Effects.Database.Tables.SitePageRevisionsSpec qualified as SitePageRevisions
import Effects.Database.Tables.SitePagesSpec qualified as SitePages
import Effects.Database.Tables.StagedUploadsSpec qualified as StagedUploads
import Effects.Database.Tables.StationIdsSpec qualified as StationIds
import Effects.Database.Tables.UserMetadataSpec qualified as UserMetadata
import Effects.Database.Tables.UserRoleSpec qualified as UserRole
import Effects.DiffSpec qualified as Diff
import Effects.MarkdownSpec qualified as Markdown
import Effects.MimeTypeValidationSpec qualified as MimeTypeValidation
import Effects.StagedUploadsSpec qualified as StagedUploadsEffects
import System.Environment (lookupEnv)
import Test.Database.Setup (withTmpPG)
import Test.Hspec
import Test.Hspec.Api.Formatters.V3 (specdoc, useFormatter)
import Test.Hspec.Runner (Config (..), hspecWith)
import Test.Hspec.Runner qualified as TR
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  mText <- lookupEnv "TEST_CONCURRENCY"
  let maxResources :: Int
      maxResources = fromMaybe 4 (mText >>= readMaybe)
  let cfg =
        useFormatter ("specdoc", specdoc) $
          TR.defaultConfig
            { configConcurrentJobs = Just maxResources
            }

  -- Pure tests that don't need database
  hspecWith cfg $ parallel $ do
    Cookie.spec
    Domains.spec
    ScheduleDiff.spec
    ContentSanitization.spec
    Diff.spec
    Markdown.spec
    Slug.spec
    StorageBackend.spec
    FileUpload.spec
    MimeTypeValidation.spec
    UserRole.spec
    StagedUploadsEffects.spec

  -- Database-dependent tests
  withTmpPG $ hspecWith cfg $ parallel $ do
    -- DB Models - Core Tables
    UserMetadata.spec
    Shows.spec
    Episodes.spec
    CurrentlyAiring.spec
    BlogPosts.spec
    BlogTags.spec
    Events.spec
    ShowHost.spec
    ShowSchedule.spec
    ShowScheduleMissingEpisodes.spec
    StagedUploads.spec

    -- DB Models - Tags
    ShowTags.spec
    ShowBlogTags.spec

    -- DB Models - Show Blog Posts
    ShowBlogPosts.spec

    -- DB Models - CRUD Tables
    EpisodeTrack.spec
    StationIds.spec
    EphemeralUploads.spec
    SitePages.spec
    SitePageRevisions.spec

    -- DB Models - Special Tables
    PlaybackHistory.spec

    -- DB Models - Auth Tokens
    EmailVerificationTokens.spec
    PasswordResetTokens.spec

    -- Handler Integration Tests
    Combinators.spec
    HomeHandler.spec
    EventsHandler.spec
    EventHandler.spec
    ShowsHandler.spec
    EpisodeEditHandler.spec
    DashboardEpisodesEditPostHandler.spec
    DashboardEpisodesDeleteHandler.spec
    DashboardEpisodesGetHandler.spec
    DashboardEpisodesRedirectHandler.spec
    DashboardEpisodesSlugGetHandler.spec
    DashboardShowsGetHandler.spec
    DashboardShowsNewGetHandler.spec
    DashboardShowsSlugGetHandler.spec
    DashboardShowsSlugEditGetHandler.spec
    DashboardShowsNewPostHandler.spec
    DashboardShowsEditPostHandler.spec
    DashboardShowsEpisodeNewPostHandler.spec
    DashboardShowsDeleteHandler.spec
    DashboardBlogsGetHandler.spec
    DashboardBlogsNewGetHandler.spec
    DashboardBlogsNewPostHandler.spec
    DashboardBlogsSlugGetHandler.spec
    DashboardBlogsEditGetHandler.spec
    DashboardBlogsEditPostHandler.spec
    DashboardBlogsDeleteHandler.spec
    BlogGetHandler.spec
    BlogPostGetHandler.spec
    ScheduleHandler.spec
    ShowDetailHandler.spec
    ShowBlogHandler.spec
    ShowBlogPostHandler.spec
    ShowEpisodeHandler.spec
    DashboardStationBlogGetHandler.spec
    DashboardStationBlogNewGetHandler.spec
    DashboardStationBlogNewPostHandler.spec
    DashboardStationBlogSlugGetHandler.spec
    DashboardStationBlogEditGetHandler.spec
    DashboardStationBlogEditPostHandler.spec
    DashboardStationBlogDeleteHandler.spec
    DashboardEventsGetHandler.spec
    DashboardEventsNewGetHandler.spec
    DashboardEventsNewPostHandler.spec
    DashboardEventsSlugGetHandler.spec
    DashboardEventsEditGetHandler.spec
    DashboardEventsEditPostHandler.spec
    DashboardEventsDeleteHandler.spec
    DashboardEventsFeatureHandler.spec
    DashboardUsersGetHandler.spec
    DashboardUsersDetailGetHandler.spec
    DashboardUsersEditGetHandler.spec
    DashboardUsersEditPostHandler.spec
    DashboardUsersDeleteHandler.spec
    DashboardUsersRolePatchHandler.spec
    DashboardUsersSuspendHandler.spec
    DashboardUsersUnsuspendHandler.spec
    DashboardProfileEditGetHandler.spec
    DashboardProfileEditPostHandler.spec
    DashboardSitePagesGetHandler.spec
    DashboardSitePagesEditGetHandler.spec
    DashboardSitePagesEditPostHandler.spec
    DashboardSitePagesHistoryGetHandler.spec
    DashboardSitePagesRevisionGetHandler.spec
    DashboardSitePagesRestorePostHandler.spec
    DashboardEphemeralUploadsGetHandler.spec
    DashboardEphemeralUploadsNewGetHandler.spec
    DashboardEphemeralUploadsEditGetHandler.spec
    DashboardEphemeralUploadsDeleteHandler.spec
    DashboardEphemeralUploadsFlagHandler.spec
    DashboardEphemeralUploadsUnflagHandler.spec
    DashboardStationIdsGetHandler.spec
    DashboardStationIdsNewGetHandler.spec
    DashboardStationIdsDeleteHandler.spec
    DashboardStreamSettingsEpisodeSearchHandler.spec
    DashboardMissingEpisodesGetHandler.spec
    UserLoginHandler.spec
    UserRegisterHandler.spec
    UserForgotPasswordHandler.spec
