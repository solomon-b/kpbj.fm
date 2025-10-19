module Main where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Effects.Database.Tables.BlogPostsSpec qualified as BlogPosts
import Effects.Database.Tables.EpisodesSpec qualified as Episodes
import Effects.Database.Tables.EventsSpec qualified as Events
import Effects.Database.Tables.ShowsSpec qualified as Shows
import Effects.Database.Tables.UserMetadataSpec qualified as UserMetadata
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

  withTmpPG $ hspecWith cfg $ parallel $ do
    -- DB Models - Core Tables
    UserMetadata.spec
    Shows.spec
    Episodes.spec
    BlogPosts.spec
    Events.spec
