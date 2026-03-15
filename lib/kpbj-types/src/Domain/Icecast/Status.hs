-- | Icecast JSON status parsing.
--
-- Shared between the web service (live listener display) and the
-- listener-snapshots cron job.
module Domain.Icecast.Status
  ( parseListenerCount,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------

-- | Parse total listener count from Icecast JSON status response.
--
-- Icecast returns @icestats.source@ as either:
--
--   - A JSON object (single mountpoint)
--   - A JSON array (multiple mountpoints)
--
-- Sums listeners across all sources. Returns 'Nothing' if the
-- response is malformed or contains no sources with a @listeners@ field.
parseListenerCount :: LBS.ByteString -> Maybe Int32
parseListenerCount body = do
  json <- Aeson.decode body
  case json of
    Object root -> do
      Object icestats <- KeyMap.lookup "icestats" root
      let sources = case KeyMap.lookup "source" icestats of
            Just (Object src) -> [src]
            Just (Array arr) -> [src | Object src <- toList arr]
            _ -> []
      case mapMaybe listenersFrom sources of
        [] -> Nothing
        counts -> Just (sum counts)
    _ -> Nothing
  where
    listenersFrom src = do
      Number n <- KeyMap.lookup (fromText "listeners") src
      pure (round n)
