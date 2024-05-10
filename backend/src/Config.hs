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
import Crypto.JOSE qualified as JOSE
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.Text.Encoding.Base64 (decodeBase64Lenient)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Data.Word (Word16)
import GHC.Generics
import Text.Megaparsec

--------------------------------------------------------------------------------

data Environment = Development | Production
  deriving (Generic, Show)
  deriving anyclass (ToJSON, ConfigParser, DefaultSource)
  deriving (ConfigSource, ValueParser) via (Value Environment)

--------------------------------------------------------------------------------

data WarpConfig = WarpConfig
  { warpConfigPort :: Int,
    warpConfigTimeout :: Int,
    warpConfigServerName :: ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts [StripPrefix "warpConfig", ToUpper] WarpConfig)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data ObservabilityConfig = ObservabilityConfig
  { observabilityConfigVerbosity :: Verbosity,
    observabilityConfigExporter :: AppExporter
  }
  deriving stock (Generic, Show)
  deriving
    (ConfigSource, ConfigParser)
    via (ConfigOpts [StripPrefix "observabilityConfig", ToUpper] ObservabilityConfig)

instance DefaultSource ObservabilityConfig where
  defaults "observabilityConfigVerbosity" = Just "Quiet"
  defaults _ = Nothing

data Verbosity = Quiet | Loud
  deriving stock (Generic, Show)
  deriving anyclass (DefaultSource)
  deriving (ConfigSource, ConfigParser) via (Value Verbosity)

data AppExporter = StdOut
  deriving (Generic, Show)
  deriving anyclass (DefaultSource)
  deriving (ConfigSource, ConfigParser) via (Value AppExporter)

--------------------------------------------------------------------------------

newtype JwtConfig = JwtConfig {getJwk :: JOSE.JWK}
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (DefaultSource, ConfigParser)
  deriving (ConfigSource) via (Value JwtConfig)

instance ValueParser JwtConfig where
  parser :: Parser JwtConfig
  parser = do
    x <- takeRest
    case Aeson.eitherDecode @JwtConfig (Text.Lazy.Encoding.encodeUtf8 $ Text.Lazy.fromStrict $ decodeBase64Lenient x) of
      Left err -> fail err
      Right jwk -> pure jwk

--------------------------------------------------------------------------------

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig,
    appConfigPostgresSettings :: PostgresConfig,
    appConfigEnvironment :: Environment,
    appConfigObservability :: ObservabilityConfig,
    appConfigJwtConfig :: JwtConfig
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
