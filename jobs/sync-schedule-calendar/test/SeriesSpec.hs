module SeriesSpec (spec) where

--------------------------------------------------------------------------------

import Data.Time (DayOfWeek (..), TimeOfDay (..))
import Domain.Types.Recurrence (airsOn, recurrenceFromRow)
import Effects.Database.Tables.ShowSchedule (CalendarSeriesRow (..))
import Gen (day, genDay, genRecurrence, genSeriesKey, weeklySaturday)
import Hedgehog (assert, forAll, (===))
import Series
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "SeriesKey" $ do
    it "parses what it renders" $ hedgehog $ do
      key <- forAll genSeriesKey
      parseSeriesKey (renderSeriesKey key) === Just key

    it "renders the validity id and the kind" $ do
      renderSeriesKey (SeriesKey 123 Primary) `shouldBe` "123-primary"
      renderSeriesKey (SeriesKey 123 Replay) `shouldBe` "123-replay"

    it "rejects text that is not a key" $
      map parseSeriesKey ["", "123", "primary", "x-primary", "123-other", "-5-primary"]
        `shouldBe` replicate 6 Nothing

  describe "seriesFromRow" $ do
    it "gives a primary and a replay of the same length for an overnight slot" $
      map (\s -> (sKey s, sStart s, sLengthMinutes s)) (seriesFromRow overnightRow)
        `shouldBe` [ (SeriesKey 7 Primary, TimeOfDay 22 0 0, 120),
                     (SeriesKey 7 Replay, TimeOfDay 10 0 0, 120)
                   ]

    it "gives only a primary when the row has no replay time" $
      map (skKind . sKey) (seriesFromRow overnightRow {csrReplayStartTime = Nothing})
        `shouldBe` [Primary]

    it "copies the show fields" $
      map (\s -> (sTitle s, sSlug s, sDescription s, sHosts s, sShowLive s)) (take 1 (seriesFromRow overnightRow))
        `shouldBe` [("Late Show", "late-show", Just "Words.", ["Ada"], True)]

  describe "classify" $ do
    let today = day 2026 11 16
    it "is Live for an open window of a live show" $
      classify today weeklySaturday `shouldBe` Live

    it "is Live for a window that closes after today" $
      classify today weeklySaturday {sUntil = Just (day 2026 12 1)} `shouldBe` Live

    it "is Ended on the day before a window that closes today" $
      classify today weeklySaturday {sUntil = Just today} `shouldBe` Ended (day 2026 11 15)

    it "is Ended for a window that closed in the past" $
      classify today weeklySaturday {sUntil = Just (day 2026 6 1)} `shouldBe` Ended (day 2026 5 31)

    it "is Ended yesterday for an open window of a show that is not live" $
      classify today weeklySaturday {sShowLive = False} `shouldBe` Ended (day 2026 11 15)

    it "keeps the earlier end for a closed window of a show that is not live" $
      classify today weeklySaturday {sShowLive = False, sUntil = Just (day 2026 6 1)}
        `shouldBe` Ended (day 2026 5 31)

    it "is Empty for a cancelled pending window" $
      classify today weeklySaturday {sFrom = day 2026 12 5, sUntil = Just (day 2026 12 5)}
        `shouldBe` Empty

    it "is Empty for a window with no airing inside" $
      classify today weeklySaturday {sFrom = day 2026 11 10, sUntil = Just (day 2026 11 13)}
        `shouldBe` Empty

    it "is Empty for a pending window of a show that is not live" $
      classify today weeklySaturday {sFrom = day 2026 12 5, sShowLive = False} `shouldBe` Empty

  describe "airings" $ do
    it "lists the Saturdays of an open weekly window" $
      take 3 (airings weeklySaturday) `shouldBe` [day 2026 1 3, day 2026 1 10, day 2026 1 17]

    it "stops before the exclusive end" $
      airings weeklySaturday {sUntil = Just (day 2026 1 17)} `shouldBe` [day 2026 1 3, day 2026 1 10]

    it "gives only dates that the recurrence covers, inside the window" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      from <- forAll genDay
      let series = weeklySaturday {sRecurrence = recurrence, sFrom = from}
          dates = take 20 (airings series)
      length dates === 20
      assert (all (airsOn recurrence) dates)
      assert (all (>= from) dates)

    it "finds the first airing of a week-5 show after a month with no week 5" $
      firstAiring weeklySaturday {sRecurrence = recurrenceFromRow Sunday [5], sFrom = day 2027 2 1}
        `shouldBe` Just (day 2027 5 30)

  describe "clipOverlaps" $ do
    let first = weeklySaturday {sFrom = day 2026 1 1, sUntil = Just (day 2026 6 1)}
        second = weeklySaturday {sKey = SeriesKey 2 Primary, sFrom = day 2026 5 1, sUntil = Just (day 2026 9 1)}
    it "moves the start of a later window to the end of an earlier one" $
      map sFrom (clipOverlaps [second, first]) `shouldMatchList` [day 2026 1 1, day 2026 6 1]

    it "makes a later window empty when an earlier window is open" $ do
      let clipped = clipOverlaps [first {sUntil = Nothing}, second]
      [classify (day 2026 11 16) s | s <- clipped, sKey s == SeriesKey 2 Primary] `shouldBe` [Empty]

    it "does not change windows of different templates" $ do
      let other = second {sTemplateId = 2}
      clipOverlaps [first, other] `shouldMatchList` [first, other]

    it "does not clip a replay against a primary" $ do
      let replay = second {sKey = SeriesKey 2 Replay}
      clipOverlaps [first, replay] `shouldMatchList` [first, replay]

    it "does not change windows that do not overlap" $ do
      let later = second {sFrom = day 2026 6 1}
      clipOverlaps [first, later] `shouldMatchList` [first, later]

    it "gives the same result for each input order when two windows start on one date" $ do
      let sameStart = second {sFrom = sFrom first}
          clipped = [first, sameStart {sFrom = day 2026 6 1}]
      clipOverlaps [first, sameStart] `shouldMatchList` clipped
      clipOverlaps [sameStart, first] `shouldMatchList` clipped

--------------------------------------------------------------------------------

-- | Saturday 22:00 to midnight, with a replay at 10:00.
overnightRow :: CalendarSeriesRow
overnightRow =
  CalendarSeriesRow
    { csrValidityId = 7,
      csrTemplateId = 3,
      csrDayOfWeek = Saturday,
      csrWeeksOfMonth = [1, 2, 3, 4, 5],
      csrStartTime = TimeOfDay 22 0 0,
      csrEndTime = TimeOfDay 0 0 0,
      csrReplayStartTime = Just (TimeOfDay 10 0 0),
      csrEffectiveFrom = day 2026 1 3,
      csrEffectiveUntil = Nothing,
      csrShowLive = True,
      csrShowTitle = "Late Show",
      csrShowSlug = "late-show",
      csrShowDescription = Just "Words.",
      csrHostNames = ["Ada"]
    }
