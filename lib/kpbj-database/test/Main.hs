module Main where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Domain.Types.FileUploadSpec qualified as FileUpload
import Domain.Types.SlugSpec qualified as Slug
import Domain.Types.StorageBackendSpec qualified as StorageBackend
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
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import Test.Database.Setup (withTmpPG)
import Test.Hspec
import Test.Hspec.Api.Formatters.V3 (specdoc, useFormatter)
import Test.Hspec.Runner (Config (..), hspecWith)
import Test.Hspec.Runner qualified as TR
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Set MIGRATIONS_DIR if not already set (migrations live in services/web/)
  mMigDir <- lookupEnv "MIGRATIONS_DIR"
  case mMigDir of
    Just _ -> pure ()
    Nothing -> do
      cwd <- getCurrentDirectory
      -- Walk up from lib/kpbj-database to project root, then into services/web/migrations
      let migrationsPath = cwd </> ".." </> ".." </> "services" </> "web" </> "migrations"
      absPath <- makeAbsolute migrationsPath
      setEnv "MIGRATIONS_DIR" absPath

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
    Slug.spec
    StorageBackend.spec
    FileUpload.spec
    UserRole.spec

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
