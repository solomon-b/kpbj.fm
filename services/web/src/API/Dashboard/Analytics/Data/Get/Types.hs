{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Analytics.Data.Get.Types
  ( AnalyticsData (..),
    ListenerData (..),
    ArchivePlayData (..),
    TopEpisode (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data AnalyticsData = AnalyticsData
  { listeners :: ListenerData,
    archivePlays :: ArchivePlayData,
    topEpisodes :: [TopEpisode]
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

data ListenerData = ListenerData
  { labels :: [Text],
    listenerData :: [Double],
    peak :: Int,
    avg :: Double
  }
  deriving stock (Generic)

instance Aeson.ToJSON ListenerData where
  toJSON ld =
    Aeson.object
      [ "labels" Aeson..= ld.labels,
        "data" Aeson..= ld.listenerData,
        "peak" Aeson..= ld.peak,
        "avg" Aeson..= ld.avg
      ]

data ArchivePlayData = ArchivePlayData
  { labels :: [Text],
    playData :: [Int],
    total :: Int
  }
  deriving stock (Generic)

instance Aeson.ToJSON ArchivePlayData where
  toJSON apd =
    Aeson.object
      [ "labels" Aeson..= apd.labels,
        "data" Aeson..= apd.playData,
        "total" Aeson..= apd.total
      ]

data TopEpisode = TopEpisode
  { rank :: Int,
    title :: Text,
    showTitle :: Text,
    plays :: Int
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)
