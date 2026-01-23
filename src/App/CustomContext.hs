{-# OPTIONS_GHC -Wno-orphans #-}

module App.CustomContext
  ( -- * Custom Context
    CustomContext (..),
    initCustomContext,

    -- * Re-exports
    StorageContext (..),
    StorageBackend (..),
    AnalyticsConfig (..),
    GoogleAnalyticsId (..),
    SmtpConfig (..),
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Analytics (AnalyticsConfig (..), GoogleAnalyticsId (..), initAnalyticsConfig)
import App.Context (AppContext (..))
import App.Smtp (SmtpConfig (..), initSmtpConfig)
import App.Storage (StorageBackend (..), StorageContext (..), initStorageContext)
import Control.Monad.IO.Class (MonadIO)
import Data.Has qualified as Has

--------------------------------------------------------------------------------

-- | Custom application context containing all subsystem configurations.
--
-- This is the type passed to 'App.runApp' and made available to handlers
-- via the 'Has' typeclass.
data CustomContext = CustomContext
  { storageContext :: StorageContext,
    analyticsConfig :: AnalyticsConfig,
    smtpConfig :: Maybe SmtpConfig
  }

--------------------------------------------------------------------------------
-- Has instances for CustomContext

instance Has.Has StorageContext CustomContext where
  getter = storageContext
  modifier f ctx = ctx {storageContext = f (storageContext ctx)}

instance Has.Has AnalyticsConfig CustomContext where
  getter = analyticsConfig
  modifier f ctx = ctx {analyticsConfig = f (analyticsConfig ctx)}

instance Has.Has (Maybe SmtpConfig) CustomContext where
  getter = smtpConfig
  modifier f ctx = ctx {smtpConfig = f (smtpConfig ctx)}

instance Has.Has StorageBackend CustomContext where
  getter = storageBackend . storageContext
  modifier f ctx =
    ctx
      { storageContext =
          (storageContext ctx)
            { storageBackend = f (storageBackend (storageContext ctx))
            }
      }

instance Has.Has (Maybe AWS.Env) CustomContext where
  getter = awsEnv . storageContext
  modifier f ctx =
    ctx
      { storageContext =
          (storageContext ctx)
            { awsEnv = f (awsEnv (storageContext ctx))
            }
      }

instance Has.Has (Maybe GoogleAnalyticsId) CustomContext where
  getter = googleAnalyticsId . analyticsConfig
  modifier f ctx =
    ctx
      { analyticsConfig =
          (analyticsConfig ctx)
            { googleAnalyticsId = f (googleAnalyticsId (analyticsConfig ctx))
            }
      }

--------------------------------------------------------------------------------
-- Orphan Has instances for AppContext CustomContext

instance Has.Has StorageContext (AppContext CustomContext) where
  getter = storageContext . appCustom
  modifier f ctx = ctx {appCustom = (appCustom ctx) {storageContext = f (storageContext (appCustom ctx))}}

instance Has.Has AnalyticsConfig (AppContext CustomContext) where
  getter = analyticsConfig . appCustom
  modifier f ctx = ctx {appCustom = (appCustom ctx) {analyticsConfig = f (analyticsConfig (appCustom ctx))}}

instance Has.Has StorageBackend (AppContext CustomContext) where
  getter = Has.getter @StorageBackend . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @StorageBackend f (appCustom ctx)}

instance Has.Has (Maybe AWS.Env) (AppContext CustomContext) where
  getter = Has.getter @(Maybe AWS.Env) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe AWS.Env) f (appCustom ctx)}

instance Has.Has (Maybe GoogleAnalyticsId) (AppContext CustomContext) where
  getter = Has.getter @(Maybe GoogleAnalyticsId) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe GoogleAnalyticsId) f (appCustom ctx)}

instance Has.Has (Maybe SmtpConfig) (AppContext CustomContext) where
  getter = Has.getter @(Maybe SmtpConfig) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe SmtpConfig) f (appCustom ctx)}

--------------------------------------------------------------------------------

-- | Initialize the complete custom context.
--
-- Loads all subsystem configurations from environment variables.
initCustomContext :: (MonadIO m) => m CustomContext
initCustomContext = do
  storage <- initStorageContext
  analytics <- initAnalyticsConfig
  smtp <- initSmtpConfig
  pure
    CustomContext
      { storageContext = storage,
        analyticsConfig = analytics,
        smtpConfig = smtp
      }
