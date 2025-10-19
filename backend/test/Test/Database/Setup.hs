module Test.Database.Setup where

--------------------------------------------------------------------------------

import Control.Exception (throw, throwIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (toStrict)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.Options (Options (..))
import Database.Postgres.Temp as PGCfg (Config (connectionOptions))
import Database.Postgres.Temp as PGTmp
  ( CommandLineArgs (keyBased),
    Config (initDbConfig),
    ProcessConfig (commandLine),
    defaultConfig,
    toConnectionOptions,
    withConfig,
  )
import System.Process.Typed as PT (ExitCode (..), ProcessConfig, proc, readProcess, setEnv)
import Test.Database.Monad

--------------------------------------------------------------------------------

tmpDbUsername :: String
tmpDbUsername = "kpbj-test-user"

tmpDbConfig :: PGCfg.Config
tmpDbConfig =
  PGTmp.defaultConfig
    { initDbConfig =
        pure
          mempty
            { commandLine =
                mempty
                  { keyBased =
                      Map.fromList
                        [ ("--username=", Just tmpDbUsername),
                          ("--no-sync", Nothing)
                        ]
                  }
            },
      connectionOptions = (connectionOptions PGTmp.defaultConfig) {user = pure tmpDbUsername}
    }

withTmpPG :: IO () -> IO ()
withTmpPG action = do
  res <- withConfig tmpDbConfig $ \db -> do
    let connOpts = toConnectionOptions db
    socketHost <- case getLast $ host connOpts of
      Nothing -> throw $ TestSetupException "Temporary test database should provide a unix socket host"
      Just host -> pure host
    socketPort <- case getLast $ port connOpts of
      Nothing -> throw $ TestSetupException "Temporary test database should provide a unix socket port"
      Just host -> pure $ show host

    -- Scaffold template db
    uuid <- nextRandom
    let dbname = "kpbj-test-template-db-" <> toString uuid
        connString =
          "postgresql://"
            <> tmpDbUsername
            <> "@unixsocket"
            <> ":"
            <> socketPort
            <> "/"
            <> dbname
            <> "?host="
            <> socketHost
    -- TODO: Investigate a better way to do migrations
    readProcessHandleErr $
      proc
        "psql"
        [ "-U",
          tmpDbUsername,
          "-d",
          "postgres",
          "-h",
          socketHost,
          "-p",
          socketPort,
          "-c",
          "CREATE DATABASE \"" <> dbname <> "\" WITH IS_TEMPLATE TRUE"
        ]
    let migration = setEnv [("DATABASE_URL", connString)] $ proc "sqlx" ["migrate", "run"]
    readProcessHandleErr migration

    void $
      writeOnce readOnlyTestDBConfig $
        TestDBInitConfig
          { testDBInitConfigTemplateName = dbname,
            testDBInitConfigHost = socketHost,
            testDBInitConfigPort = socketPort,
            testDBInitConfigUser = tmpDbUsername
          }
    action

  case res of
    Left e -> throwIO e
    Right _ -> pure ()

-- TODO: Look for a better function that does something like this. Or pull out for better reuse
readProcessHandleErr ::
  (MonadIO m) =>
  PT.ProcessConfig stdin stdoutIgnored stderrIgnored ->
  m ()
readProcessHandleErr process = do
  (ec, _, stde) <- readProcess process
  when (ec /= ExitSuccess) $
    throw $
      TestSetupException $
        decodeUtf8 (toStrict stde)
