module SyncSpec (spec) where

--------------------------------------------------------------------------------

import Control.Monad.State (State, modify, runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays)
import Event
import Gen (day, genRecurrence, weeklySaturday)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rrule (Rule (..))
import Series (Series (..), SeriesKey (..), SeriesKind (..), SeriesState (..), classify, renderSeriesKey)
import Sync
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

today :: Day
today = day 2026 11 16

render :: Series -> Maybe CalendarEvent
render = seriesToEvent "https://www.kpbj.fm"

plan :: [Series] -> [RemoteEvent] -> [Action]
plan = planSync today render

-- | The event of 'weeklySaturday'.
event :: CalendarEvent
event = fromMaybe (error "weeklySaturday has an airing") (render weeklySaturday)

-- | An event in the calendar with the content that the job wrote.
remoteOf :: Text -> CalendarEvent -> RemoteEvent
remoteOf eventId e =
  RemoteSeries eventId (Just (ceKey e)) (Just e) (maybe Unbounded EndsOn (ruleLastDay (ceRule e)))

-- | An event that a person made, with no key.
handMade :: Text -> Extent -> RemoteEvent
handMade eventId = RemoteSeries eventId Nothing Nothing

spec :: Spec
spec = do
  describe "planSync, live series" $ do
    it "inserts the event of a live series that the calendar does not have" $
      plan [weeklySaturday] [] `shouldBe` [Insert event]

    it "does nothing when the calendar has the same event" $
      plan [weeklySaturday] [remoteOf "a" event] `shouldBe` []

    it "updates an event with other content" $
      plan [weeklySaturday] [remoteOf "a" event {ceSummary = "Old Title"}] `shouldBe` [Update "a" event]

    it "updates an event whose content does not parse" $
      plan [weeklySaturday] [RemoteSeries "a" (Just (ceKey event)) Nothing Unbounded]
        `shouldBe` [Update "a" event]

  describe "planSync, ended series" $ do
    let ended = weeklySaturday {sUntil = Just (day 2026 6 1)}
        lastDay = day 2026 5 31
    it "sets the end of an event that is still open" $
      plan [ended] [remoteOf "a" event]
        `shouldBe` [SetLastDay "a" (Rule (sRecurrence ended) (Just lastDay))]

    it "does nothing when the end is correct, even if other content differs" $
      plan [ended] [remoteOf "a" event {ceSummary = "Old Title", ceRule = Rule (sRecurrence ended) (Just lastDay)}]
        `shouldBe` []

    it "does not add an event for a series that ended" $
      plan [ended] [] `shouldBe` []

    it "does not add an event for a closed window of a show that is not live" $
      plan [ended {sShowLive = False}] [] `shouldBe` []

    it "ends the event of a show that is not live any more" $
      plan [weeklySaturday {sShowLive = False}] [remoteOf "a" event]
        `shouldBe` [SetLastDay "a" (Rule (sRecurrence weeklySaturday) (Just (day 2026 11 15)))]

    -- An open window of a show that is not live ends "yesterday" on each
    -- date. The end that the first run set must stay, or the event grows by
    -- one day each day and shows airings that did not air.
    it "does not move the end of an event forward on a later date" $ do
      let notLive = weeklySaturday {sShowLive = False}
          endedYesterday = event {ceRule = Rule (sRecurrence notLive) (Just (day 2026 11 15))}
      planSync (day 2026 11 30) render [notLive] [remoteOf "a" endedYesterday] `shouldBe` []

    it "moves the end of an event back when it is too late" $
      plan [ended] [remoteOf "a" event {ceRule = Rule (sRecurrence ended) (Just (day 2026 8 1))}]
        `shouldBe` [SetLastDay "a" (Rule (sRecurrence ended) (Just lastDay))]

  describe "planSync, empty series" $ do
    let cancelled = weeklySaturday {sFrom = day 2026 12 5, sUntil = Just (day 2026 12 5)}
    it "deletes the event of a cancelled pending window" $
      plan [cancelled] [remoteOf "a" event {ceStart = ceStart event}] `shouldBe` [Delete "a"]

    it "does nothing when the calendar has no event for it" $
      plan [cancelled] [] `shouldBe` []

  describe "planSync, events that the job does not know" $ do
    it "deletes a hand-made event that is today or later" $
      plan [] [handMade "h" (EndsOn (day 2026 11 20))] `shouldBe` [Delete "h"]

    it "deletes a hand-made event that is today" $
      plan [] [handMade "h" (EndsOn today)] `shouldBe` [Delete "h"]

    it "leaves a hand-made event that ended before today" $
      plan [] [handMade "h" (EndsOn (day 2026 11 10))] `shouldBe` []

    it "deletes a hand-made recurring event with no end" $
      plan [] [handMade "h" Unbounded] `shouldBe` [Delete "h"]

    it "deletes an open event whose key matches no series" $
      plan [] [remoteOf "a" event] `shouldBe` [Delete "a"]

    it "leaves an ended event whose key matches no series" $
      plan [] [remoteOf "a" event {ceRule = Rule (sRecurrence weeklySaturday) (Just (day 2026 5 31))}]
        `shouldBe` []

    it "keeps the event with the smallest id when two have one key" $
      plan [weeklySaturday] [remoteOf "b" event, remoteOf "a" event] `shouldBe` [Delete "b"]

  -- Nobody must edit the calendar by hand. The job does not repair one
  -- airing. It reports the edit, and a person corrects it.
  describe "single airings that a person changed" $ do
    let deleted = RemoteException "a_1" "a" (day 2026 11 21) True
        moved = RemoteException "a_2" "a" today False
        past = RemoteException "a_3" "a" (day 2026 11 14) True

    it "plans no action for an airing that a person deleted or moved" $
      plan [weeklySaturday] [remoteOf "a" event, deleted, moved, past] `shouldBe` []

    it "reports the changed airings of today and later dates" $
      handEdits today [remoteOf "a" event, deleted, moved, past] `shouldBe` [deleted, moved]

    it "reports nothing for a calendar with no changed airing" $
      handEdits today [remoteOf "a" event, handMade "h" Unbounded] `shouldBe` []

  describe "planSync, order" $
    it "puts the ends first, then deletes, updates, and inserts" $ do
      let other = weeklySaturday {sKey = SeriesKey 2 Primary, sTemplateId = 2}
          ended = weeklySaturday {sKey = SeriesKey 3 Primary, sTemplateId = 3, sUntil = Just (day 2026 6 1)}
          newSeries = weeklySaturday {sKey = SeriesKey 4 Primary, sTemplateId = 4}
          eventOf s = fromMaybe (error "has an airing") (render s)
      plan
        [newSeries, weeklySaturday, other, ended]
        [ remoteOf "a" event {ceSummary = "Old"},
          remoteOf "b" (eventOf other),
          remoteOf "c" (eventOf ended) {ceRule = Rule (sRecurrence ended) Nothing},
          handMade "h" Unbounded
        ]
        `shouldBe` [ SetLastDay "c" (Rule (sRecurrence ended) (Just (day 2026 5 31))),
                     Delete "h",
                     Update "a" event,
                     Insert (eventOf newSeries)
                   ]

  describe "planSync, properties" $ do
    it "gives no actions for a calendar that holds the events of the live series" $ hedgehog $ do
      series <- forAll genSeriesList
      let live = filter ((== Live) . classify today) series
          remote = [remoteOf (renderSeriesKey (ceKey e)) e | e <- mapMaybe render live]
      plan series remote === []

    it "gives no actions after its own actions are applied" $ hedgehog $ do
      series <- forAll genSeriesList
      model <- forAll (genModel series)
      let synced = foldl (flip applyAction) model (plan series (Map.elems model))
      plan series (Map.elems synced) === []

  describe "runActions" $
    it "runs every action and returns the failures" $ do
      let actions = [Delete "a", Delete "b", Delete "c"]
          execute :: Action -> State [Action] (Either Text ())
          execute action = do
            modify (<> [action])
            pure (if action == Delete "b" then Left "b failed" else Right ())
      runState (runActions execute actions) [] `shouldBe` (["b failed"], actions)

  describe "describeAction" $
    it "starts with the kind of the action" $
      map (Text.takeWhile (/= ' ') . describeAction) [Insert event, Update "a" event, SetLastDay "a" (ceRule event), Delete "a"]
        `shouldBe` ["insert", "update", "end", "delete"]

--------------------------------------------------------------------------------
-- Model

-- | A calendar as a map from event id to event. It holds no exceptions.
type Model = Map Text RemoteEvent

applyAction :: Action -> Model -> Model
applyAction = \case
  Insert e -> let eventId = "new-" <> renderSeriesKey (ceKey e) in Map.insert eventId (remoteOf eventId e)
  Update eventId e -> Map.insert eventId (remoteOf eventId e)
  SetLastDay eventId rule -> Map.adjust (setRule rule) eventId
  Delete eventId -> Map.delete eventId
  where
    setRule :: Rule -> RemoteEvent -> RemoteEvent
    setRule rule remote = case remote of
      RemoteSeries {} ->
        remote
          { rsEvent = (\e -> e {ceRule = rule}) <$> rsEvent remote,
            rsExtent = maybe Unbounded EndsOn (ruleLastDay rule)
          }
      RemoteException {} -> remote

-- | Series with different keys and templates. Each one is live, ended, or
-- empty on 'today'.
genSeriesList :: Gen [Series]
genSeriesList = do
  count <- Gen.integral (Range.linear 0 8)
  traverse genSeries [1 .. count]
  where
    genSeries n = do
      recurrence <- genRecurrence
      kind <- Gen.element [Primary, Replay]
      from <- (`addDays` today) <$> Gen.integral (Range.linearFrom 0 (-400) 60)
      until' <-
        Gen.choice
          [ pure Nothing,
            pure (Just from),
            Just . (`addDays` from) <$> Gen.integral (Range.linear 1 500)
          ]
      live <- Gen.frequency [(4, pure True), (1, pure False)]
      pure
        weeklySaturday
          { sKey = SeriesKey n kind,
            sTemplateId = n,
            sRecurrence = recurrence,
            sFrom = from,
            sUntil = until',
            sShowLive = live
          }

-- | A calendar in any state: for each series the correct event, an old event,
-- a duplicate, or no event, plus events that the job does not know.
genModel :: [Series] -> Gen Model
genModel series = do
  known <- concat <$> traverse genKnown (mapMaybe (\s -> (,) s <$> render s) series)
  unknown <- Gen.list (Range.linear 0 4) genExtent
  let events = known <> zipWith (\n extent -> handMade ("hand-" <> Text.pack (show n)) extent) [1 :: Int ..] unknown
  pure (Map.fromList [(rsId e, e) | e <- events])
  where
    genKnown (s, e) = do
      let eventId = "id-" <> renderSeriesKey (sKey s)
          old = e {ceSummary = "Old Title", ceRule = Rule (sRecurrence s) Nothing}
      Gen.element
        [ [],
          [remoteOf eventId e],
          [remoteOf eventId old],
          [remoteOf eventId e, remoteOf (eventId <> "-copy") old],
          [RemoteSeries eventId (Just (sKey s)) Nothing Unbounded]
        ]

    genExtent = Gen.choice [pure Unbounded, EndsOn . (`addDays` today) <$> Gen.integral (Range.linearFrom 0 (-30) 30)]
