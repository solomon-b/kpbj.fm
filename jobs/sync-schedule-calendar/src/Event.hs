-- | The calendar event of a slot series, and its Google Calendar JSON form.
--
-- 'CalendarEvent' is the value that the sync compares. 'seriesToEvent' gives
-- the event that the calendar must hold. 'decodeRemoteEvent' reads the event
-- that the calendar holds now back into the same type.
--
-- See <https://developers.google.com/workspace/calendar/api/v3/reference/events>.
module Event
  ( -- * Event
    CalendarEvent (..),
    seriesToEvent,
    encodeEvent,

    -- * Events in the calendar
    RemoteEvent (..),
    Extent (..),
    decodeRemoteEvent,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Object, Parser, parseEither)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (..), UTCTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Timezone (pacificDay, pacificToUtc, utcToPacific)
import Rrule (Rule (..), parseRule, printRule)
import Series (Series (..), SeriesKey (..), SeriesKind (..), airings, firstAiring, parseSeriesKey, renderSeriesKey)

--------------------------------------------------------------------------------
-- Event

-- | One recurring calendar event. The times are in station time.
data CalendarEvent = CalendarEvent
  { ceKey :: SeriesKey,
    ceSummary :: Text,
    ceDescription :: Text,
    -- | The start of the first airing.
    ceStart :: LocalTime,
    ceEnd :: LocalTime,
    ceRule :: Rule
  }
  deriving stock (Show, Eq)

-- | The event of a series. A series with no airing has no event.
--
-- The first argument is the base URL of the website, with no slash at the end.
--
-- The event starts at the first airing whose start time exists. A start time
-- in the spring daylight-saving gap does not exist, and playout has dead air
-- for that airing. Google would move such a start forward by one hour.
--
-- The end is the start plus the real slot length. Google uses the length of
-- the first airing for each airing, so a wall-clock length would be wrong for
-- all of them when the first airing crosses a clock change.
seriesToEvent :: Text -> Series -> Maybe CalendarEvent
seriesToEvent baseUrl s@Series {..} = do
  _ <- firstAiring s
  -- The gap is on one date of the year, so one of two airings is outside it.
  start <- listToMaybe [local | d <- take 2 (airings s), let local = LocalTime d sStart, exists local]
  pure
    CalendarEvent
      { ceKey = sKey,
        ceSummary = case skKind sKey of
          Primary -> sTitle
          Replay -> sTitle <> " (Replay)",
        ceDescription = Text.intercalate "\n\n" (hosts <> description <> [baseUrl <> "/shows/" <> sSlug]),
        ceStart = start,
        ceEnd = utcToPacific (addUTCTime (fromIntegral (sLengthMinutes * 60)) (pacificToUtc start)),
        ceRule = Rule sRecurrence (pred <$> sUntil)
      }
  where
    -- A local time exists when the trip through UTC returns it.
    exists :: LocalTime -> Bool
    exists local = utcToPacific (pacificToUtc local) == local

    hosts :: [Text]
    hosts = ["Hosted by " <> Text.intercalate ", " sHosts | not (null sHosts)]

    -- The show description is Markdown. The event carries it as stored.
    description :: [Text]
    description = [text | Just text <- [sDescription], not (Text.null (Text.strip text))]

-- | The request body for @events.insert@ and @events.update@.
--
-- The events are @transparent@, so they do not mark a subscriber as busy.
encodeEvent :: CalendarEvent -> Value
encodeEvent CalendarEvent {..} =
  object
    [ "summary" .= ceSummary,
      "description" .= ceDescription,
      "start" .= time ceStart,
      "end" .= time ceEnd,
      "recurrence" .= [printRule ceRule],
      "transparency" .= ("transparent" :: Text),
      "extendedProperties" .= object ["private" .= object ["kpbjSeries" .= renderSeriesKey ceKey]]
    ]
  where
    -- A dateTime with no offset is a local time in the given zone.
    time :: LocalTime -> Value
    time local =
      object
        [ "dateTime" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" local,
          "timeZone" .= ("America/Los_Angeles" :: Text)
        ]

--------------------------------------------------------------------------------
-- Events in the calendar

-- | The last station date on which an event has an airing.
data Extent = EndsOn Day | Unbounded
  deriving stock (Show, Eq)

-- | An event as the calendar holds it.
data RemoteEvent
  = -- | A single event or the parent of a recurring event.
    RemoteSeries
      { rsId :: Text,
        rsKey :: Maybe SeriesKey,
        -- | The content, when the event has a key and every field parses. An
        -- event that a person changed to a form that the job does not write
        -- has 'Nothing' here, and the job replaces it.
        rsEvent :: Maybe CalendarEvent,
        rsExtent :: Extent
      }
  | -- | One airing of a recurring event that a person changed or deleted.
    RemoteException
      { rxId :: Text,
        rxParentId :: Text,
        -- | The station date of the airing before the change.
        rxOriginalDay :: Day,
        rxCancelled :: Bool
      }
  deriving stock (Show, Eq)

-- | Read one item of an @events.list@ response. It fails only when the item has
-- no @id@, or is an exception with no original start time.
decodeRemoteEvent :: Value -> Either Text RemoteEvent
decodeRemoteEvent = first Text.pack . parseEither (Aeson.withObject "event" event)
  where
    event :: Object -> Parser RemoteEvent
    event o = do
      eventId <- o .: "id"
      o .:? "recurringEventId" >>= \case
        Just parentId -> exception o eventId parentId
        Nothing -> pure (series o eventId)

-- | A cancelled exception has only its id, its parent id, and its original
-- start time. So this parser reads no other field.
exception :: Object -> Text -> Text -> Parser RemoteEvent
exception o eventId parentId = do
  original <- o .: "originalStartTime"
  originalDay <- (pacificDay <$> original .: "dateTime") <|> original .: "date"
  status <- o .:? "status" .!= ("confirmed" :: Text)
  pure (RemoteException eventId parentId originalDay (status == "cancelled"))

series :: Object -> Text -> RemoteEvent
series o eventId =
  RemoteSeries
    { rsId = eventId,
      rsKey = key,
      rsEvent = CalendarEvent <$> key <*> Just summary <*> Just description <*> start <*> end <*> rule,
      rsExtent = extent
    }
  where
    field :: (Object -> Parser a) -> Maybe a
    field parser = either (const Nothing) Just (parseEither parser o)

    key :: Maybe SeriesKey
    key = parseSeriesKey =<< field (\x -> x .: "extendedProperties" >>= (.: "private") >>= (.: "kpbjSeries"))

    summary, description :: Text
    summary = fromMaybe "" (field (.: "summary"))
    description = fromMaybe "" (field (.: "description"))

    instant :: Aeson.Key -> Maybe UTCTime
    instant name = field (\x -> x .: name >>= (.: "dateTime"))

    zone :: Aeson.Key -> Maybe Text
    zone name = field (\x -> x .: name >>= (.: "timeZone"))

    -- Google expands the rule in the zone of the event. So a time in another
    -- zone is not a time of the schedule, even when the instant is the same.
    stationTime :: Aeson.Key -> Maybe LocalTime
    stationTime name = do
      "America/Los_Angeles" <- zone name
      utcToPacific <$> instant name

    start, end :: Maybe LocalTime
    start = stationTime "start"
    end = stationTime "end"

    recurrence :: [Text]
    recurrence = fromMaybe [] (field (.: "recurrence"))

    rule :: Maybe Rule
    rule = case recurrence of
      [line] -> either (const Nothing) Just (parseRule line)
      _ -> Nothing

    -- The end date of an all-day event is exclusive.
    allDayEnd :: Maybe Day
    allDayEnd = pred <$> field (\x -> x .: "end" >>= (.: "date"))

    extent :: Extent
    extent = case (recurrence, rule) of
      ([], _) -> maybe Unbounded EndsOn ((pacificDay <$> instant "end") <|> allDayEnd)
      (_, Just (Rule _ (Just lastDay))) -> EndsOn lastDay
      _ -> Unbounded
