module Utils
  ( fromRightM,
    fromMaybeM,
    catEithers,
    partitionEithers,
    escapeJsString,
  )
where

--------------------------------------------------------------------------------

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m

fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM f m = maybe f pure =<< m

catEithers :: [Either e a] -> [a]
catEithers = mapMaybe (either (const Nothing) Just)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (ls, rs) = (a : ls, rs)
    right b (ls, rs) = (ls, b : rs)

-- | Escape a Text value for use inside a JavaScript string literal.
-- Escapes backslashes, quotes, newlines, tabs, and template literal backticks.
escapeJsString :: Text -> Text
escapeJsString = Text.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\'' = "\\'"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '`' = "\\`"
    escapeChar c = Text.singleton c
