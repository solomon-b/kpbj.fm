{-# LANGUAGE PackageImports #-}

-- | Test infrastructure for running handler-level integration tests.
--
-- Bridges the existing 'TestDBConfig' from database tests to a full
-- 'AppContext CustomContext', enabling tests to run 'AppM' actions
-- (including 'ExceptT HandlerError AppM') against a real test database.
module Test.Handler.Monad
  ( bracketAppM,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..), Hostname (..), Verbosity (..), WarpConfig (..))
import App.Context (AppContext (..))
import App.CustomContext (AnalyticsConfig (..), CustomContext (..), PlayoutSecret (..), StorageContext (..), StreamConfig (..), WebhookConfig (..))
import Control.Monad.Catch (bracket)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (StorageBackend (..), defaultLocalConfig)
import Hasql.Connection.Setting qualified as HSQL.Setting
import Hasql.Connection.Setting.Connection qualified as HSQL.Connection
import Hasql.Connection.Setting.Connection.Param qualified as HSQL.Params
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Pool.Config qualified as HSQL.Pool.Config
import Test.Database.Monad (TestDBConfig (..), noOpLoggerEnv)
import "kpbj-api" App.Monad (AppM)
import "web-server-core" App.Monad qualified as Core (AppM (..))

--------------------------------------------------------------------------------

-- | Run an 'AppM' action against a real test database.
--
-- Constructs a minimal 'AppContext CustomContext' from the test database config,
-- runs the action, then releases the pool.
--
-- @
-- bracketAppM cfg $ do
--   result <- execQuery SomeTable.getAll
--   liftIO $ result \`shouldSatisfy\` isRight
-- @
bracketAppM :: TestDBConfig -> AppM a -> IO a
bracketAppM cfg action =
  bracket (acquireTestPool cfg) HSQL.Pool.release $ \pool -> do
    let appCtx = mkTestAppContext pool
    Core.runAppM action appCtx

--------------------------------------------------------------------------------
-- Internal helpers

acquireTestPool :: TestDBConfig -> IO HSQL.Pool.Pool
acquireTestPool TestDBConfig {..} =
  let hsqlSettings =
        [ HSQL.Setting.connection $
            HSQL.Connection.params
              [ HSQL.Params.host $ Text.pack testDBConfigHost,
                HSQL.Params.port $ read testDBConfigPort,
                HSQL.Params.dbname $ Text.pack testDBConfigDBname,
                HSQL.Params.user $ Text.pack testDBConfigUser
              ]
        ]
      poolSettings = HSQL.Pool.Config.settings $ pure $ HSQL.Pool.Config.staticConnectionSettings hsqlSettings
   in HSQL.Pool.acquire poolSettings

mkTestAppContext :: HSQL.Pool.Pool -> AppContext CustomContext
mkTestAppContext pool =
  AppContext
    { appDbPool = pool,
      appHostname = Hostname "localhost",
      appEnvironment = Development,
      appVerbosity = Quiet,
      appLoggerEnv = noOpLoggerEnv,
      appWarpConfig = testWarpConfig,
      appCustom = testCustomContext
    }

testWarpConfig :: WarpConfig
testWarpConfig =
  WarpConfig
    { warpConfigPort = 4000,
      warpConfigTimeout = 30,
      warpConfigServerName = "test"
    }

testCustomContext :: CustomContext
testCustomContext =
  CustomContext
    { storageContext =
        StorageContext
          { storageBackend = LocalStorage defaultLocalConfig,
            awsEnv = Nothing
          },
      analyticsConfig = AnalyticsConfig Nothing,
      smtpConfig = Nothing,
      playoutSecret = PlayoutSecret Nothing,
      webhookConfig = WebhookDisabled,
      streamConfig =
        StreamConfig
          { scStreamUrl = "http://localhost:8000/stream",
            scMetadataUrl = "http://localhost:8000/status-json.xsl"
          }
    }
