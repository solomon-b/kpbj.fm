module EventSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay (..), diffUTCTime)
import Domain.Types.Recurrence (everyWeek, recurrenceFromRow, recurring)
import Domain.Types.Timezone (pacificToUtc)
import Event
import Gen (day, genCalendarEvent, weeklySaturday)
import Hedgehog (forAll, (===))
import Rrule (Rule (..))
import Series (Series (..), SeriesKey (..), SeriesKind (..))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

baseUrl :: Text
baseUrl = "https://www.kpbj.fm"

spec :: Spec
spec = do
  describe "seriesToEvent" $ do
    it "maps a weekly slot" $
      seriesToEvent baseUrl weeklySaturday
        `shouldBe` Just
          CalendarEvent
            { ceKey = SeriesKey 1 Primary,
              ceSummary = "Night Drive",
              ceDescription = "Hosted by Ada, Grace\n\nTwo hours of slow music.\n\nhttps://www.kpbj.fm/shows/night-drive",
              ceStart = LocalTime (day 2026 1 3) (TimeOfDay 18 0 0),
              ceEnd = LocalTime (day 2026 1 3) (TimeOfDay 20 0 0),
              ceRule = Rule (recurring Saturday everyWeek) Nothing
            }

    it "starts a first-and-third slot on its first airing in the window" $ do
      let series = weeklySaturday {sRecurrence = recurrenceFromRow Saturday [1, 3], sFrom = day 2026 11 8}
      fmap ceStart (seriesToEvent baseUrl series)
        `shouldBe` Just (LocalTime (day 2026 11 21) (TimeOfDay 18 0 0))

    it "ends an overnight slot on the next date" $ do
      let series = weeklySaturday {sStart = TimeOfDay 22 0 0, sLengthMinutes = 120}
      fmap ceEnd (seriesToEvent baseUrl series)
        `shouldBe` Just (LocalTime (day 2026 1 4) (TimeOfDay 0 0 0))

    it "marks a replay in the title" $ do
      let series = weeklySaturday {sKey = SeriesKey 1 Replay, sStart = TimeOfDay 10 0 0}
      fmap (\e -> (ceSummary e, ceStart e)) (seriesToEvent baseUrl series)
        `shouldBe` Just ("Night Drive (Replay)", LocalTime (day 2026 1 3) (TimeOfDay 10 0 0))

    it "ends the rule on the day before an exclusive window end" $
      fmap (ruleLastDay . ceRule) (seriesToEvent baseUrl weeklySaturday {sUntil = Just (day 2026 12 1)})
        `shouldBe` Just (Just (day 2026 11 30))

    it "gives no event for a window with no airing" $
      seriesToEvent baseUrl weeklySaturday {sFrom = day 2026 11 10, sUntil = Just (day 2026 11 13)}
        `shouldBe` Nothing

    it "has only the link when the show has no hosts and no description" $
      fmap ceDescription (seriesToEvent baseUrl weeklySaturday {sHosts = [], sDescription = Nothing})
        `shouldBe` Just "https://www.kpbj.fm/shows/night-drive"

    it "omits a blank description" $
      fmap ceDescription (seriesToEvent baseUrl weeklySaturday {sHosts = [], sDescription = Just "  \n"})
        `shouldBe` Just "https://www.kpbj.fm/shows/night-drive"

    -- Google uses the length of the first airing for each airing. So the
    -- length must be the real slot length, also when the first airing is on a
    -- date that changes the clock. 2027-03-14 and 2026-11-01 are such dates.
    it "gives the real length when the first airing crosses the spring clock change" $ do
      let series = sundayAt (TimeOfDay 1 0 0) (day 2027 3 14)
      fmap (\e -> (ceStart e, ceEnd e)) (seriesToEvent baseUrl series)
        `shouldBe` Just (LocalTime (day 2027 3 14) (TimeOfDay 1 0 0), LocalTime (day 2027 3 14) (TimeOfDay 4 0 0))

    it "gives the real length when the first airing crosses the autumn clock change" $ do
      let series = sundayAt (TimeOfDay 0 30 0) (day 2026 11 1)
      fmap (\e -> diffUTCTime (pacificToUtc (ceEnd e)) (pacificToUtc (ceStart e))) (seriesToEvent baseUrl series)
        `shouldBe` Just 7200

    -- 02:30 does not exist on the spring date. Playout has dead air for that
    -- airing. Google would move the start to 03:30, and the job would then see
    -- a difference on each run.
    it "starts at the next airing when the first one is in the spring gap" $ do
      let series = sundayAt (TimeOfDay 2 30 0) (day 2027 3 14)
      fmap ceStart (seriesToEvent baseUrl series)
        `shouldBe` Just (LocalTime (day 2027 3 21) (TimeOfDay 2 30 0))

    it "gives no event when the only airing of a window is in the spring gap" $
      seriesToEvent baseUrl (sundayAt (TimeOfDay 2 30 0) (day 2027 3 14)) {sUntil = Just (day 2027 3 15)}
        `shouldBe` Nothing

  describe "encodeEvent" $
    it "writes the fields that Google needs" $
      fmap encodeEvent (seriesToEvent baseUrl weeklySaturday {sHosts = [], sDescription = Nothing})
        `shouldBe` Just
          ( object
              [ "summary" .= ("Night Drive" :: Text),
                "description" .= ("https://www.kpbj.fm/shows/night-drive" :: Text),
                "start" .= object ["dateTime" .= ("2026-01-03T18:00:00" :: Text), "timeZone" .= ("America/Los_Angeles" :: Text)],
                "end" .= object ["dateTime" .= ("2026-01-03T20:00:00" :: Text), "timeZone" .= ("America/Los_Angeles" :: Text)],
                "recurrence" .= ["RRULE:FREQ=WEEKLY;BYDAY=SA" :: Text],
                "transparency" .= ("transparent" :: Text),
                "extendedProperties" .= object ["private" .= object ["kpbjSeries" .= ("1-primary" :: Text)]]
              ]
          )

  describe "decodeRemoteEvent" $ do
    it "reads back what encodeEvent wrote, with the instants that Google returns" $ hedgehog $ do
      event <- forAll genCalendarEvent
      decodeRemoteEvent (asGoogleReturns event)
        === Right
          RemoteSeries
            { rsId = "x",
              rsKey = Just (ceKey event),
              rsEvent = Just event,
              rsExtent = maybe Unbounded EndsOn (ruleLastDay (ceRule event))
            }

    it "reads an all-day event with a key as an event of other content" $ do
      let allDay =
            object
              [ "id" .= ("x" :: Text),
                "start" .= object ["date" .= ("2026-11-21" :: Text)],
                "end" .= object ["date" .= ("2026-11-22" :: Text)],
                "extendedProperties" .= object ["private" .= object ["kpbjSeries" .= ("1-primary" :: Text)]]
              ]
      decodeRemoteEvent allDay
        `shouldBe` Right (RemoteSeries "x" (Just (SeriesKey 1 Primary)) Nothing (EndsOn (day 2026 11 21)))

    -- Google expands the rule in the zone of the event. An event in another
    -- zone can have the same first instant and different airings.
    it "reads an event in another zone as an event of other content" $ hedgehog $ do
      event <- forAll genCalendarEvent
      let moved = setZone "America/Phoenix" (asGoogleReturns event)
      fmap rsEvent (decodeRemoteEvent moved) === Right Nothing
      fmap rsKey (decodeRemoteEvent moved) === Right (Just (ceKey event))

    it "reads a single event with no key, with the date that it ends" $ do
      let single =
            object
              [ "id" .= ("y" :: Text),
                "summary" .= ("Station closed" :: Text),
                "start" .= object ["dateTime" .= ("2026-11-20T09:00:00-08:00" :: Text)],
                "end" .= object ["dateTime" .= ("2026-11-20T10:00:00-08:00" :: Text)]
              ]
      decodeRemoteEvent single `shouldBe` Right (RemoteSeries "y" Nothing Nothing (EndsOn (day 2026 11 20)))

    it "reads a recurring event with a rule that the job does not write as unbounded" $ do
      let other =
            object
              [ "id" .= ("z" :: Text),
                "start" .= object ["dateTime" .= ("2026-11-20T09:00:00-08:00" :: Text)],
                "end" .= object ["dateTime" .= ("2026-11-20T10:00:00-08:00" :: Text)],
                "recurrence" .= ["RRULE:FREQ=DAILY;COUNT=3" :: Text]
              ]
      decodeRemoteEvent other `shouldBe` Right (RemoteSeries "z" Nothing Nothing Unbounded)

    it "reads a cancelled exception from its three fields" $ do
      let cancelled =
            object
              [ "id" .= ("abc_20261121T020000Z" :: Text),
                "status" .= ("cancelled" :: Text),
                "recurringEventId" .= ("abc" :: Text),
                "originalStartTime" .= object ["dateTime" .= ("2026-11-20T18:00:00-08:00" :: Text)]
              ]
      decodeRemoteEvent cancelled
        `shouldBe` Right (RemoteException "abc_20261121T020000Z" "abc" (day 2026 11 20) True)

    it "reads a moved exception as not cancelled" $ do
      let moved =
            object
              [ "id" .= ("abc_20261128T020000Z" :: Text),
                "status" .= ("confirmed" :: Text),
                "recurringEventId" .= ("abc" :: Text),
                "originalStartTime" .= object ["dateTime" .= ("2026-11-27T18:00:00-08:00" :: Text)],
                "start" .= object ["dateTime" .= ("2026-11-27T19:00:00-08:00" :: Text)],
                "end" .= object ["dateTime" .= ("2026-11-27T21:00:00-08:00" :: Text)]
              ]
      decodeRemoteEvent moved
        `shouldBe` Right (RemoteException "abc_20261128T020000Z" "abc" (day 2026 11 27) False)

    it "fails only when the event has no id" $
      decodeRemoteEvent (object ["summary" .= ("x" :: Text)]) `shouldSatisfy` isLeft

--------------------------------------------------------------------------------

-- | A weekly Sunday show of 120 minutes, from the given date.
sundayAt :: TimeOfDay -> Day -> Series
sundayAt start from =
  weeklySaturday {sRecurrence = recurring Sunday everyWeek, sStart = start, sLengthMinutes = 120, sFrom = from}

-- | Replace the zone of the start time of an event body.
setZone :: Text -> Value -> Value
setZone zone = \case
  Aeson.Object fields -> Aeson.Object (KeyMap.mapWithKey (\name -> if name == "start" then inStart else id) fields)
  other -> other
  where
    inStart :: Value -> Value
    inStart = \case
      Aeson.Object start -> Aeson.Object (KeyMap.insert "timeZone" (Aeson.String zone) start)
      other -> other

-- | The body of 'encodeEvent' as Google returns it: with an id, and with each
-- time as an instant.
asGoogleReturns :: CalendarEvent -> Value
asGoogleReturns event = case encodeEvent event of
  Aeson.Object fields ->
    Aeson.Object
      ( KeyMap.insert "id" (Aeson.String "x")
          . KeyMap.insert "start" (instant (ceStart event))
          . KeyMap.insert "end" (instant (ceEnd event))
          $ fields
      )
  other -> other
  where
    instant :: LocalTime -> Value
    instant local =
      object ["dateTime" .= pacificToUtc local, "timeZone" .= ("America/Los_Angeles" :: Text)]
