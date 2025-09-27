module Domain.Types.Show where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, DiffTime, UTCTime)
import Servant qualified

--------------------------------------------------------------------------------

data ShowStatus = Active | Inactive | Hiatus | Archived
  deriving (Prelude.Show, Eq, Enum, Bounded)

data ShowFrequency = Weekly | Biweekly | Monthly | Occasional | OneTime
  deriving (Prelude.Show, Eq, Enum, Bounded)

data HostRole = Host | CoHost | Guest
  deriving (Prelude.Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------

data Show = Show
  { showId :: Int64,
    showTitle :: Text,
    showSlug :: Text,
    showDescription :: Text,
    showGenre :: Maybe Text,
    showLogoUrl :: Maybe Text,
    showBannerUrl :: Maybe Text,
    showStatus :: ShowStatus,
    showFrequency :: ShowFrequency,
    showDurationMinutes :: Maybe Int,
    showCreatedAt :: UTCTime,
    showUpdatedAt :: UTCTime
  }
  deriving (Prelude.Show, Eq)

data ShowHost = ShowHost
  { showHostShowId :: Int64,
    showHostUserId :: Int64,
    showHostRole :: HostRole,
    showHostIsPrimary :: Bool,
    showHostJoinedAt :: UTCTime,
    showHostLeftAt :: Maybe UTCTime
  }
  deriving (Prelude.Show, Eq)

data ShowSchedule = ShowSchedule
  { scheduleId :: Int64,
    scheduleShowId :: Int64,
    scheduleDayOfWeek :: Int, -- 0=Sunday, 1=Monday, etc.
    scheduleStartTime :: DiffTime,
    scheduleEndTime :: DiffTime,
    scheduleTimezone :: Text,
    scheduleIsActive :: Bool,
    scheduleEffectiveFrom :: Day,
    scheduleEffectiveUntil :: Maybe Day,
    scheduleCreatedAt :: UTCTime
  }
  deriving (Prelude.Show, Eq)

--------------------------------------------------------------------------------

instance Servant.FromHttpApiData ShowStatus where
  parseQueryParam = \case
    "active" -> Right Active
    "inactive" -> Right Inactive
    "hiatus" -> Right Hiatus
    "archived" -> Right Archived
    _ -> Left "Invalid show status"

instance Servant.ToHttpApiData ShowStatus where
  toQueryParam = \case
    Active -> "active"
    Inactive -> "inactive"
    Hiatus -> "hiatus"
    Archived -> "archived"

instance Servant.FromHttpApiData ShowFrequency where
  parseQueryParam = \case
    "weekly" -> Right Weekly
    "biweekly" -> Right Biweekly
    "monthly" -> Right Monthly
    "occasional" -> Right Occasional
    "one-time" -> Right OneTime
    _ -> Left "Invalid show frequency"

instance Servant.ToHttpApiData ShowFrequency where
  toQueryParam = \case
    Weekly -> "weekly"
    Biweekly -> "biweekly"
    Monthly -> "monthly"
    Occasional -> "occasional"
    OneTime -> "one-time"

instance Servant.FromHttpApiData HostRole where
  parseQueryParam = \case
    "host" -> Right Host
    "co-host" -> Right CoHost
    "guest" -> Right Guest
    _ -> Left "Invalid host role"

instance Servant.ToHttpApiData HostRole where
  toQueryParam = \case
    Host -> "host"
    CoHost -> "co-host"
    Guest -> "guest"
