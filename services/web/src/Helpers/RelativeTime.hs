-- | Relative time formatting for human-readable timestamps.
--
-- Converts UTC timestamps to relative time strings like "3 days ago",
-- "2 weeks ago", "just now", etc.
module Helpers.RelativeTime
  ( formatRelativeTime,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

--------------------------------------------------------------------------------

-- | Format the difference between two timestamps as relative time.
--
-- The first argument is the current time, the second is the timestamp to format.
--
-- Examples:
--
-- > formatRelativeTime now (now - 30 seconds) == "just now"
-- > formatRelativeTime now (now - 5 minutes) == "5 minutes ago"
-- > formatRelativeTime now (now - 3 days) == "3 days ago"
formatRelativeTime :: UTCTime -> UTCTime -> Text
formatRelativeTime now publishedAt =
  let diff = diffUTCTime now publishedAt
   in formatDiff diff

formatDiff :: NominalDiffTime -> Text
formatDiff diff
  | diff < 60 = "just now"
  | diff < 3600 = showPlural (floor (diff / 60)) "minute"
  | diff < 86400 = showPlural (floor (diff / 3600)) "hour"
  | diff < 604800 = showPlural (floor (diff / 86400)) "day"
  | diff < 2592000 = showPlural (floor (diff / 604800)) "week"
  | diff < 31536000 = showPlural (floor (diff / 2592000)) "month"
  | otherwise = showPlural (floor (diff / 31536000)) "year"

showPlural :: Int -> Text -> Text
showPlural 1 unit = "1 " <> unit <> " ago"
showPlural n unit = Text.pack (show n) <> " " <> unit <> "s ago"
