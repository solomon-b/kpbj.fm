{-# LANGUAGE DuplicateRecordFields #-}

module API.Dashboard.Analytics.Get.Types
  ( -- * Range
    AnalyticsRange (..),
    defaultRange,

    -- * View data
    AnalyticsData (..),
    ListenerData (..),
    ArchivePlayData (..),
    ChartSeries (..),
    TopEpisode (..),
    TopDimensionEntry (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

--------------------------------------------------------------------------------
-- Range

-- | The time range of the analytics page, from the @range@ query parameter.
data AnalyticsRange = Range24h | Range7d | Range30d | Range90d | RangeAll
  deriving stock (Eq, Show, Enum, Bounded)

instance ToHttpApiData AnalyticsRange where
  toUrlPiece :: AnalyticsRange -> Text
  toUrlPiece = \case
    Range24h -> "24h"
    Range7d -> "7d"
    Range30d -> "30d"
    Range90d -> "90d"
    RangeAll -> "all"

instance FromHttpApiData AnalyticsRange where
  parseUrlPiece :: Text -> Either Text AnalyticsRange
  parseUrlPiece = \case
    "24h" -> Right Range24h
    "7d" -> Right Range7d
    "30d" -> Right Range30d
    "90d" -> Right Range90d
    "all" -> Right RangeAll
    other -> Left $ "Unknown analytics range: " <> other

-- | The range of a request with no @range@ query parameter.
defaultRange :: AnalyticsRange
defaultRange = Range7d

--------------------------------------------------------------------------------
-- View data

-- | All the data that the analytics page shows for one range.
data AnalyticsData = AnalyticsData
  { listeners :: ListenerData,
    archivePlays :: ArchivePlayData,
    topEpisodes :: [TopEpisode],
    topReferrers :: [TopDimensionEntry],
    topCountries :: [TopDimensionEntry],
    topCities :: [TopDimensionEntry]
  }

data ListenerData = ListenerData
  { series :: ChartSeries,
    peak :: Int,
    avg :: Double
  }

data ArchivePlayData = ArchivePlayData
  { series :: ChartSeries,
    total :: Int
  }

-- | The points of one chart. The page puts this JSON in the @data-chart@
-- attribute of the chart canvas.
data ChartSeries = ChartSeries
  { labels :: [Text],
    values :: [Double]
  }

instance Aeson.ToJSON ChartSeries where
  toJSON :: ChartSeries -> Aeson.Value
  toJSON cs = Aeson.object ["labels" Aeson..= cs.labels, "values" Aeson..= cs.values]

data TopEpisode = TopEpisode
  { rank :: Int,
    title :: Text,
    showTitle :: Text,
    plays :: Int,
    url :: Text
  }
  deriving stock (Eq, Show)

-- | One row in a top-N table from Google Analytics (referrers, countries,
-- or cities).
data TopDimensionEntry = TopDimensionEntry
  { rank :: Int,
    label :: Text,
    sessions :: Int
  }
  deriving stock (Eq, Show)
