{-# LANGUAGE QuasiQuotes #-}

-- | Application base URL derivation from context.
module App.BaseUrl (baseUrl) where

--------------------------------------------------------------------------------

import App.Config (Environment (..), Hostname (..), WarpConfig (..))
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Derive the application base URL from context.
--
-- Uses 'Hostname', 'Environment', and 'WarpConfig' from 'AppM':
--
--   * Development: @http:\/\/hostname:port@
--   * Staging/Production: @https:\/\/hostname@
baseUrl :: AppM Text
baseUrl = do
  Hostname hostname <- asks Has.getter
  env <- asks (Has.getter @Environment)
  WarpConfig {warpConfigPort = port} <- asks Has.getter
  pure $ case env of
    Development -> [i|http://#{hostname}:#{port}|]
    Staging -> [i|https://#{hostname}|]
    Production -> [i|https://#{hostname}|]
