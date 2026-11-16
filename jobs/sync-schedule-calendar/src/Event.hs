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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (..), NominalDiffTime, UTCTime, ZonedTime, addUTCTime, diffLocalTime, diffUTCTime, minutesToTimeZone, utc, utcToLocalTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Timezone (pacificDay, pacificToUtc, utcToPacific)
import Rrule (Rule (..), parseRule, printRule)
import Series (Series (..), SeriesKey (..), SeriesKind (..), firstAiring, parseSeriesKey, renderSeriesKey)

--------------------------------------------------------------------------------
-- Event

-- | One recurring calendar event.
data CalendarEvent = CalendarEvent
  { ceKey :: SeriesKey,
    ceSummary :: Text,
    ceDescription :: Text,
    -- | The start of the first airing, in station time.
    ceStart :: LocalTime,
    -- | The real length of the first airing. A local end time can not replace
    -- it. On the autumn clock-change date, each time from 01:00 to 01:59
    -- occurs twice.
    ceLength :: NominalDiffTime,
    ceRule :: Rule
  }
  deriving stock (Show, Eq)

-- | The event of a series. A series with no airing has no event.
--
-- The first argument is the base URL of the website, with no slash at the end.
--
-- The event starts at the first airing. 'Series.airings' gives no date on which the
-- start time is in the spring daylight-saving gap. Google would move such a
-- start forward by one hour.
--
-- The length is the real slot length. Google uses the length of the first
-- airing for each airing, so a wall-clock length would be wrong for all of
-- them when the first airing crosses a clock change.
seriesToEvent :: Text -> Series -> Maybe CalendarEvent
seriesToEvent baseUrl s@Series {..} = do
  firstDay <- firstAiring s
  pure
    CalendarEvent
      { ceKey = sKey,
        ceSummary = case skKind sKey of
          Primary -> sTitle
          Replay -> sTitle <> " (Replay)",
        ceDescription = Text.intercalate "\n\n" (hosts <> description <> [baseUrl <> "/shows/" <> sSlug]),
        ceStart = LocalTime firstDay sStart,
        ceLength = fromIntegral (sLengthMinutes * 60),
        ceRule = Rule sRecurrence (pred <$> sUntil)
      }
  where
    hosts :: [Text]
    hosts = ["Hosted by " <> Text.intercalate ", " sHosts | not (null sHosts)]

    -- The show description is Markdown. The event carries it as stored, but
    -- with LF line ends. A textarea gives CRLF, and a difference in how
    -- Google stores a line end would cause an update on each run.
    description :: [Text]
    description = [Text.replace "\r\n" "\n" text | Just text <- [sDescription], not (Text.null (Text.strip text))]

-- | The request body for @events.insert@ and @events.update@.
--
-- The events are @transparent@, so they do not mark a subscriber as busy.
encodeEvent :: CalendarEvent -> Value
encodeEvent CalendarEvent {..} =
  object
    [ "summary" .= ceSummary,
      "description" .= ceDescription,
      "start" .= time start,
      "end" .= time (addUTCTime ceLength start),
      "recurrence" .= [printRule ceRule],
      "transparency" .= ("transparent" :: Text),
      "extendedProperties" .= object ["private" .= object ["kpbjSeries" .= renderSeriesKey ceKey]]
    ]
  where
    start :: UTCTime
    start = pacificToUtc ceStart

    -- The dateTime has its UTC offset. A local time with no offset is
    -- ambiguous in the repeated hour of the autumn clock-change date.
    time :: UTCTime -> Value
    time instant =
      object
        [ "dateTime" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" (stationZoned instant),
          "timeZone" .= ("America/Los_Angeles" :: Text)
        ]

-- | An instant as a station time with its UTC offset.
stationZoned :: UTCTime -> ZonedTime
stationZoned instant = utcToZonedTime (minutesToTimeZone offsetMinutes) instant
  where
    offsetMinutes :: Int
    offsetMinutes = round (diffLocalTime (utcToPacific instant) (utcToLocalTime utc instant) / 60)

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
      rsEvent = CalendarEvent <$> key <*> Just summary <*> Just description <*> start <*> eventLength <*> rule,
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
    stationInstant :: Aeson.Key -> Maybe UTCTime
    stationInstant name = do
      "America/Los_Angeles" <- zone name
      instant name

    start :: Maybe LocalTime
    start = utcToPacific <$> stationInstant "start"

    eventLength :: Maybe NominalDiffTime
    eventLength = diffUTCTime <$> stationInstant "end" <*> stationInstant "start"

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
