-- | How many slots each host missed.
--
-- 'Effects.Database.Tables.ShowSchedule.getPastSlots' gives the finished
-- slots, each with its outcome and its hosts. This module counts them. A slot
-- counts one time for each host of the show on its date.
module Domain.Attendance
  ( -- * Range
    AttendanceRange (..),
    defaultRange,
    rangeStart,
    rangeLabel,

    -- * Tally
    Tally (..),
    tallySlot,
    missedPercent,

    -- * Slots
    missedSlots,
    slotsOfHost,

    -- * Hosts
    HostAttendance (..),
    hostAttendance,
  )
where

--------------------------------------------------------------------------------

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Semigroup (First (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Domain.Types.DisplayName (DisplayName)
import Effects.Database.Tables.ShowSchedule (PastSlot (..), PastSlotHost (..), SlotOutcome (..))
import Effects.Database.Tables.User qualified as User
import Servant (FromHttpApiData (..), ToHttpApiData (..))

--------------------------------------------------------------------------------
-- Range

-- | How far back the count goes, from the @range@ query parameter.
data AttendanceRange = Range30d | Range90d | Range1y | RangeAll
  deriving stock (Eq, Show, Enum, Bounded)

instance ToHttpApiData AttendanceRange where
  toUrlPiece :: AttendanceRange -> Text
  toUrlPiece = \case
    Range30d -> "30d"
    Range90d -> "90d"
    Range1y -> "1y"
    RangeAll -> "all"

instance FromHttpApiData AttendanceRange where
  parseUrlPiece :: Text -> Either Text AttendanceRange
  parseUrlPiece = \case
    "30d" -> Right Range30d
    "90d" -> Right Range90d
    "1y" -> Right Range1y
    "all" -> Right RangeAll
    other -> Left $ "Unknown attendance range: " <> other

-- | The range of a request with no @range@ query parameter.
defaultRange :: AttendanceRange
defaultRange = Range90d

-- | The first instant of a range that ends now. 'Nothing' means the range has
-- no start, so the count goes back to the start of the air record.
rangeStart ::
  -- | The current time
  UTCTime ->
  AttendanceRange ->
  Maybe UTCTime
rangeStart now = \case
  Range30d -> Just (daysAgo 30)
  Range90d -> Just (daysAgo 90)
  Range1y -> Just (daysAgo 365)
  RangeAll -> Nothing
  where
    daysAgo :: Integer -> UTCTime
    daysAgo days = addUTCTime (negate (fromInteger days * nominalDay)) now

-- | The words for a range in a sentence, for example "in the last 90 days".
rangeLabel :: AttendanceRange -> Text
rangeLabel = \case
  Range30d -> "in the last 30 days"
  Range90d -> "in the last 90 days"
  Range1y -> "in the last year"
  RangeAll -> "since the air record started"

--------------------------------------------------------------------------------
-- Tally

-- | A count of slots, and how many of them were missed.
--
-- Tallies add: the tally of two sets of slots is the sum of the two tallies.
data Tally = Tally
  { tMissed :: Int,
    tTotal :: Int
  }
  deriving stock (Eq, Show)

instance Semigroup Tally where
  (<>) :: Tally -> Tally -> Tally
  Tally missed1 total1 <> Tally missed2 total2 = Tally (missed1 + missed2) (total1 + total2)

instance Monoid Tally where
  mempty :: Tally
  mempty = Tally 0 0

-- | The tally of one slot.
tallySlot :: PastSlot -> Tally
tallySlot slot = case slot.psOutcome of
  Missed -> Tally 1 1
  Aired -> Tally 0 1

-- | The part of the slots that were missed, as a whole percent. 'Nothing' for
-- a tally of no slots.
missedPercent :: Tally -> Maybe Int
missedPercent tally
  | tally.tTotal <= 0 = Nothing
  | otherwise = Just (round (100 * fromIntegral tally.tMissed / fromIntegral tally.tTotal :: Double))

--------------------------------------------------------------------------------
-- Slots

-- | The slots with no new episode.
missedSlots :: [PastSlot] -> [PastSlot]
missedSlots = filter ((== Missed) . (.psOutcome))

-- | The slots that a user was a host of.
slotsOfHost :: User.Id -> [PastSlot] -> [PastSlot]
slotsOfHost userId = filter (any ((== userId) . (.pshUserId)) . (.psHosts))

--------------------------------------------------------------------------------
-- Hosts

-- | The slots of one host.
data HostAttendance = HostAttendance
  { haUserId :: User.Id,
    haDisplayName :: DisplayName,
    -- | The titles of the shows that the slots belong to.
    haShowTitles :: Set Text,
    haTally :: Tally
  }
  deriving stock (Eq, Show)

-- | The tally of each host with one slot or more. The host with the most
-- missed slots is first. Hosts with the same count are in name order.
--
-- A slot with two hosts counts for both. So the sum of the tallies can be
-- more than the number of slots.
hostAttendance :: [PastSlot] -> [HostAttendance]
hostAttendance slots =
  sortOn sortKey $
    fmap toHostAttendance $
      Map.toList $
        Map.fromListWith
          (<>)
          [ (host.pshUserId, (First host.pshDisplayName, Set.singleton slot.psShowTitle, tallySlot slot))
          | slot <- slots,
            host <- slot.psHosts
          ]
  where
    toHostAttendance :: (User.Id, (First DisplayName, Set Text, Tally)) -> HostAttendance
    toHostAttendance (userId, (First displayName, showTitles, tally)) =
      HostAttendance
        { haUserId = userId,
          haDisplayName = displayName,
          haShowTitles = showTitles,
          haTally = tally
        }

    sortKey :: HostAttendance -> (Down Int, Text)
    sortKey host = (Down host.haTally.tMissed, Text.toLower (display host.haDisplayName))
