module Main where

--------------------------------------------------------------------------------

import App.CookieSpec qualified as Cookie
import App.DomainsSpec qualified as Domains
import Data.Maybe (fromMaybe)
import Domain.Types.FileUploadSpec qualified as FileUpload
import Domain.Types.SlugSpec qualified as Slug
import Domain.Types.StorageBackendSpec qualified as StorageBackend
import Effects.ContentSanitizationSpec qualified as ContentSanitization
import Effects.Database.Tables.BlogPostsSpec qualified as BlogPosts
import Effects.Database.Tables.EpisodesSpec qualified as Episodes
import Effects.Database.Tables.EventsSpec qualified as Events
import Effects.Database.Tables.ShowHostSpec qualified as ShowHost
import Effects.Database.Tables.ShowScheduleSpec qualified as ShowSchedule
import Effects.Database.Tables.ShowsSpec qualified as Shows
import Effects.Database.Tables.SitePageRevisionsSpec qualified as SitePageRevisions
import Effects.Database.Tables.SitePagesSpec qualified as SitePages
import Effects.Database.Tables.StagedUploadsSpec qualified as StagedUploads
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
    BlogPosts.spec
    Events.spec
    ShowHost.spec
    ShowSchedule.spec
    StagedUploads.spec
    SitePages.spec
    SitePageRevisions.spec
