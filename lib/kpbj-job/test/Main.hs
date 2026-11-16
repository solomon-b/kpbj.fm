{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (evaluate, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Effects.Database.Execute (execQuery)
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Job (runJob)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath ((</>))
import Test.Database.Monad (TestDBConfig (..), withTestDB)
import Test.Database.Setup (withTmpPG)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldThrow)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- The migrations are in services/web. This path is correct when cabal runs
  -- the suite from lib/kpbj-job.
  mMigrationsDir <- lookupEnv "MIGRATIONS_DIR"
  case mMigrationsDir of
    Just _ -> pure ()
    Nothing -> do
      cwd <- getCurrentDirectory
      absPath <- makeAbsolute (cwd </> ".." </> ".." </> "services" </> "web" </> "migrations")
      setEnv "MIGRATIONS_DIR" absPath

  -- The examples run one at a time, because DATABASE_URL belongs to the
  -- process.
  withTmpPG (hspec spec)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Job.runJob" $ do
      it "runs a statement with execQuery" $ \cfg -> do
        setDatabaseUrl cfg
        result <- runJob "test" (execQuery selectOne)
        result `shouldBe'` [1]

      it "returns a failed statement as a Left" $ \cfg -> do
        setDatabaseUrl cfg
        result <- runJob "test" (execQuery divideByZero)
        either (const True) (const False) result `shouldBe` True

      it "exits with code 1 when DATABASE_URL is missing" $ \_ -> do
        unsetEnv "DATABASE_URL"
        runJob "test" (pure ()) `shouldThrow` (== ExitFailure 1)

      it "exits with code 1 when the database refuses the connection" $ \_ -> do
        setEnv "DATABASE_URL" "postgresql://nobody@127.0.0.1:1/none"
        runJob "test" (pure ()) `shouldThrow` (== ExitFailure 1)

      it "exits with code 1 when DATABASE_URL is not a connection string" $ \_ -> do
        setEnv "DATABASE_URL" "not a url"
        runJob "test" (pure ()) `shouldThrow` (== ExitFailure 1)

      it "exits with code 1 when an exception escapes" $ \cfg -> do
        setDatabaseUrl cfg
        runJob "test" (liftIO (throwIO (userError "boom"))) `shouldThrow` (== ExitFailure 1)

      it "exits with code 1 when pure code throws" $ \cfg -> do
        setDatabaseUrl cfg
        runJob "test" (liftIO (evaluate (error "boom" :: ()))) `shouldThrow` (== ExitFailure 1)

      it "keeps the exit code of the job" $ \cfg -> do
        setDatabaseUrl cfg
        runJob "test" (liftIO (exitWith (ExitFailure 3))) `shouldThrow` (== ExitFailure 3)
        runJob "test" (liftIO exitSuccess) `shouldThrow` (== ExitSuccess)
  where
    -- A UsageError has no Eq instance, so compare the Right value only.
    shouldBe' result expected = either (fail . show) (`shouldBe` expected) result

--------------------------------------------------------------------------------

selectOne :: Hasql.Statement () [Int32]
selectOne = fmap getOneColumn <$> interp False [sql|SELECT 1 :: int4|]

divideByZero :: Hasql.Statement () [Int32]
divideByZero = fmap getOneColumn <$> interp False [sql|SELECT 1 / 0 :: int4|]

-- | Point DATABASE_URL at the database of one example.
--
-- The form is the one that 'withTmpPG' gives to sqlx.
setDatabaseUrl :: TestDBConfig -> IO ()
setDatabaseUrl TestDBConfig {..} =
  setEnv "DATABASE_URL" $
    "postgresql://"
      <> testDBConfigUser
      <> "@unixsocket:"
      <> testDBConfigPort
      <> "/"
      <> testDBConfigDBname
      <> "?host="
      <> testDBConfigHost
