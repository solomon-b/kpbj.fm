{-# LANGUAGE DerivingVia #-}

module Config where

--------------------------------------------------------------------------------

import Cfg.Deriving (StripPrefix, StripSuffix, ToUpper)
import Cfg.Deriving.Config
import Cfg.Deriving.Value
import Cfg.Options (RootKey (..))
import Cfg.Parser
import Cfg.Source
import Cfg.Source.Default
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import GHC.Generics

--------------------------------------------------------------------------------

data Environment = Development | Production
  deriving (Generic, Show)
  deriving anyclass (ToJSON, ConfigParser, DefaultSource)
  deriving (ConfigSource, ValueParser) via (Value Environment)

data WarpConfig = WarpConfig
  { warpConfigPort :: Int,
    warpConfigTimeout :: Int,
    warpConfigServerName :: ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts [StripPrefix "warpConfig", ToUpper] WarpConfig)

data PostgresConfig = PostgresConfig
  { postgresConfigHost :: Maybe ByteString,
    postgresConfigPort :: Maybe Word16,
    postgresConfigDB :: Maybe ByteString,
    postgresConfigUser :: Maybe ByteString,
    postgresConfigPassword :: Maybe ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts [StripPrefix "postgresConfig", ToUpper] PostgresConfig)

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig,
    appConfigPostgresSettings :: PostgresConfig,
    appConfigEnvironment :: Environment
  }
  deriving stock (Generic, Show)
  deriving
    (ConfigSource, ConfigParser)
    via ( ConfigRoot
            ('TypeName [StripSuffix "Config", ToUpper])
            [StripPrefix "appConfig", StripSuffix "Settings", ToUpper]
            AppConfig
        )

instance DefaultSource AppConfig where
  defaults "appConfigEnvironment" = Just "Development"
  defaults _ = Nothing
