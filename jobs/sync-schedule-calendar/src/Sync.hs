-- | The difference between the schedule and the calendar.
--
-- 'planSync' is a pure function from the slot series and the events that the
-- calendar holds to a list of actions. The job keeps no record of what it
-- wrote. Each event carries the key of its series, so each run starts from the
-- real state of the calendar.
--
-- Two rules shape the plan:
--
-- * The job owns the calendar for today and later dates. It removes events
--   that it did not write.
-- * The past is frozen. The job never changes what the calendar shows for
--   dates before today.
--
-- Nobody must edit the calendar by hand. The job works on full events only. It
-- does not repair one airing that a person moved or deleted. 'handEdits' finds
-- such airings, so that the job can report them.
module Sync
  ( Action (..),
    planSync,
    handEdits,
    runActions,
    describeAction,
  )
where

--------------------------------------------------------------------------------

import Data.Either (lefts)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Event (CalendarEvent (..), Extent (..), RemoteEvent (..))
import Rrule (Rule (..))
import Series (Series (..), SeriesKey, SeriesState (..), classify, renderSeriesKey)

--------------------------------------------------------------------------------

-- | One change to the calendar. An event id identifies an event that exists.
data Action
  = Insert CalendarEvent
  | Update Text CalendarEvent
  | -- | Set the end of a recurring event and change nothing else.
    SetLastDay Text Rule
  | Delete Text
  deriving stock (Show, Eq)

-- | Compute the actions that make the calendar equal to the schedule.
--
-- The first argument is today in station time. The second gives the event of a
-- series, or 'Nothing' for a series with no airing. The caller must remove
-- overlaps from the series with 'Series.clipOverlaps' first.
--
-- The result has the ends first, then the deletes, the updates, and the
-- inserts. It has no action for a 'RemoteException'.
planSync :: Day -> (Series -> Maybe CalendarEvent) -> [Series] -> [RemoteEvent] -> [Action]
planSync today render series remote =
  ends <> deletes <> updates <> inserts
  where
    parents :: [RemoteEvent]
    parents = filter isSeries remote

    -- The event of each key. When two events have one key, the event with the
    -- smallest id is the event of that key.
    owners :: Map SeriesKey RemoteEvent
    owners =
      Map.fromListWith
        (\a b -> if rsId a < rsId b then a else b)
        [(key, event) | event@RemoteSeries {rsKey = Just key} <- parents]

    seriesKeys :: Set SeriesKey
    seriesKeys = Set.fromList (map sKey series)

    seriesActions :: [Action]
    seriesActions = concatMap seriesAction series

    seriesAction :: Series -> [Action]
    seriesAction s = case (classify today s, render s, Map.lookup (sKey s) owners) of
      (Live, Just event, Nothing) -> [Insert event]
      (Live, Just event, Just owner) -> [Update (rsId owner) event | rsEvent owner /= Just event]
      -- The end of an ended event only moves back. A later end would add
      -- airings to dates before today.
      (Ended lastDay, _, Just owner) ->
        [SetLastDay (rsId owner) (Rule (sRecurrence s) (Just lastDay)) | rsExtent owner `endsAfter` lastDay]
      (Ended _, _, Nothing) -> []
      -- A series with no airing has no event.
      (_, _, owner) -> [Delete (rsId event) | Just event <- [owner]]

    -- An event that is not the event of a series: it has no key, its key
    -- matches no series, or another event owns its key.
    isUnknown :: RemoteEvent -> Bool
    isUnknown event = case rsKey event of
      Just key | key `Set.member` seriesKeys -> fmap rsId (Map.lookup key owners) /= Just (rsId event)
      _ -> True

    -- The frozen past: an event that ended before today stays.
    unknownDeletes :: [Action]
    unknownDeletes =
      [Delete (rsId event) | event <- parents, isUnknown event, reachesToday (rsExtent event)]

    reachesToday :: Extent -> Bool
    reachesToday extent = extent `endsAfter` pred today

    endsAfter :: Extent -> Day -> Bool
    endsAfter extent d = case extent of
      Unbounded -> True
      EndsOn lastDay -> lastDay > d

    ends, deletes, updates, inserts :: [Action]
    ends = [action | action@SetLastDay {} <- seriesActions]
    deletes = unknownDeletes <> [action | action@Delete {} <- seriesActions]
    updates = [action | action@Update {} <- seriesActions]
    inserts = [action | action@Insert {} <- seriesActions]

isSeries :: RemoteEvent -> Bool
isSeries = \case
  RemoteSeries {} -> True
  RemoteException {} -> False

-- | The airings of today and later dates that a person moved or deleted by
-- hand. The first argument is today in station time.
--
-- The job does not change these. An airing before today is not in the result,
-- because the past is frozen.
handEdits :: Day -> [RemoteEvent] -> [RemoteEvent]
handEdits today remote = [edit | edit@RemoteException {rxOriginalDay = originalDay} <- remote, originalDay >= today]

--------------------------------------------------------------------------------

-- | Run every action and return the failures. One failure does not stop the
-- actions that follow. The next run of the job starts from the real state of
-- the calendar and finishes the work.
runActions :: (Monad m) => (Action -> m (Either e ())) -> [Action] -> m [e]
runActions execute = fmap lefts . traverse execute

-- | One line for the log. It starts with the kind of the action.
describeAction :: Action -> Text
describeAction = \case
  Insert event -> "insert " <> label event
  Update eventId event -> "update " <> eventId <> " " <> label event
  SetLastDay eventId rule -> "end " <> eventId <> maybe "" (\d -> " on " <> Text.pack (show d)) (ruleLastDay rule)
  Delete eventId -> "delete " <> eventId
  where
    label :: CalendarEvent -> Text
    label event = renderSeriesKey (ceKey event) <> " " <> ceSummary event
