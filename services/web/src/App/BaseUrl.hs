{-# LANGUAGE QuasiQuotes #-}

-- | Application base URL derivation from context.
module App.BaseUrl (baseUrl, absoluteMediaUrl) where

--------------------------------------------------------------------------------

import App.Config (Environment (..), Hostname (..))
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.StorageBackend (StorageBackend (..), buildMediaUrl)

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
  pure $ case env of
    Development -> [i|http://#{hostname}|]
    Staging -> [i|https://#{hostname}|]
    Production -> [i|https://#{hostname}|]

-- | Build an absolute URL for a stored media file.
--
-- 'buildMediaUrl' gives a full URL for S3 storage. For local storage it gives
-- a path such as @\/media\/...@, so this puts the site base URL in front.
-- Feed readers and other outside clients need the absolute form.
absoluteMediaUrl ::
  -- | The site base URL, from 'baseUrl'
  Text ->
  StorageBackend ->
  -- | The object key of the file
  Text ->
  Text
absoluteMediaUrl siteBaseUrl backend objectKey =
  case backend of
    S3Storage _ -> buildMediaUrl backend objectKey
    LocalStorage _ -> siteBaseUrl <> buildMediaUrl backend objectKey
