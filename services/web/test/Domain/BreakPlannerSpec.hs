-- | Tests for the pure daily break planner.
module Domain.BreakPlannerSpec where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (nub, sortOn)
import Data.Time (Day, UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Domain.BreakPlanner
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule (BreakKind (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Test.Hspec

--------------------------------------------------------------------------------
-- Fixtures

-- | January 10, 2025.
day :: Day
day = fromGregorian 2025 1 10

-- | Show breaks, one every 30 minutes from 10:00 UTC.
showBreaks :: Int -> [PlanBreak]
showBreaks k =
  [ PlanBreak (addUTCTime (fromIntegral (i * 1800)) (UTCTime day (secondsToDiffTime 36000))) ShowBreak
  | i <- [0 .. k - 1]
  ]

-- | A 10 second station ID that has never aired.
stationId :: StationIdCandidate
stationId = StationIdCandidate (StationIds.Id 1) 10 Nothing

-- | A 20 second underwriting announcement that has never aired.
--
-- Ids below 100 are announcements, so a test can tell them from PSAs.
creative ::
  -- | Break item id
  Int64 ->
  -- | Underwriter id
  Int64 ->
  BreakItemCandidate
creative itemId uw =
  BreakItemCandidate (BreakItems.Id itemId) BreakItems.Underwriting (Just (Underwriters.Id uw)) 20 Nothing

-- | An announcement last used at the given hour of the test day.
usedAt :: Int -> BreakItemCandidate -> BreakItemCandidate
usedAt hour c = c {bicLastUsed = Just (UTCTime day (fromIntegral (hour * 3600)))}

-- | A PSA that has never aired. Ids from 100 up are PSAs.
psa :: Int64 -> Int64 -> BreakItemCandidate
psa itemId secs = BreakItemCandidate (BreakItems.Id itemId) BreakItems.Psa Nothing secs Nothing

-- | A day with one 10 second station ID, the given PSAs, and the given
-- announcements.
input :: [PlanBreak] -> [BreakItemCandidate] -> [BreakItemCandidate] -> PlanInput
input bs psas creatives = PlanInput day bs [stationId] (psas <> creatives)

-- | The break items in each break, in break order and play order.
itemsPerBreak :: [PlanEntry] -> [[BreakItems.Id]]
itemsPerBreak entries =
  [ [i | PlanEntry b' _ (BreakItemRef i) <- sortOn pePosition entries, b' == b]
  | b <- nub (map peBoundary entries)
  ]

-- | The underwriting announcements in each break.
spotsPerBreak :: [PlanEntry] -> [[BreakItems.Id]]
spotsPerBreak = map (filter (< BreakItems.Id 100)) . itemsPerBreak

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.BreakPlanner.planDay" $ do
  describe "the template" $ do
    it "opens every break with a station ID at position 0" $ do
      let entries = planDay (input (showBreaks 3) [] [])
      [peRef e | e <- entries, pePosition e == 0]
        `shouldBe` replicate 3 (StationIdRef (StationIds.Id 1))

    it "plays the station ID, then the underwriting, then the PSAs" $ do
      let entries = planDay (input (showBreaks 1) [psa 100 30, psa 101 30] [creative 5 1])
      map peRef (sortOn pePosition entries)
        `shouldBe` [ StationIdRef (StationIds.Id 1),
                     BreakItemRef (BreakItems.Id 5),
                     BreakItemRef (BreakItems.Id 100),
                     BreakItemRef (BreakItems.Id 101)
                   ]

    it "places at most two PSAs in a break" $ do
      let entries = planDay (input (showBreaks 1) [psa i 10 | i <- [100 .. 104]] [])
      map length (itemsPerBreak entries) `shouldBe` [2]

    it "never puts underwriting in an automation break" $ do
      let bs = [PlanBreak (UTCTime day 0) AutomationBreak]
          entries = planDay (input bs [] [creative 5 1])
      itemsPerBreak entries `shouldBe` [[]]

    it "gives each break a station ID on a 46 break day" $ do
      let entries = planDay (input (showBreaks 46) [] [])
      length [() | PlanEntry _ 0 (StationIdRef _) <- entries] `shouldBe` 46

    it "places at most one station ID in a break" $ do
      let ids = [StationIdCandidate (StationIds.Id 1) 10 Nothing, StationIdCandidate (StationIds.Id 2) 8 Nothing]
          i = (input (showBreaks 1) [] []) {piStationIds = ids}
      map peRef (planDay i) `shouldBe` [StationIdRef (StationIds.Id 1)]

    it "rotates the station IDs, least recently used first" $ do
      let old = StationIdCandidate (StationIds.Id 1) 10 (Just (UTCTime day 0))
          new = StationIdCandidate (StationIds.Id 2) 10 Nothing
          i = (input (showBreaks 3) [] []) {piStationIds = [old, new]}
      [sid | PlanEntry _ _ (StationIdRef sid) <- planDay i]
        `shouldBe` map StationIds.Id [2, 1, 2]

    it "plays a station ID longer than the break, and nothing after it" $ do
      -- An old upload can be longer than the whole break. It still plays, and
      -- it leaves no time for the announcement or the PSAs.
      let longId = StationIdCandidate (StationIds.Id 1) 130 Nothing
          i = (input (showBreaks 1) [psa 100 30] [creative 5 1]) {piStationIds = [longId]}
      map peRef (planDay i) `shouldBe` [StationIdRef (StationIds.Id 1)]

  describe "underwriting" $ do
    it "places at most one announcement in a show break" $ do
      -- Three underwriters want six turns, but there are four breaks.
      let entries = planDay (input (showBreaks 4) [] [creative 5 1, creative 6 2, creative 7 3])
      map length (spotsPerBreak entries) `shouldBe` [1, 1, 1, 1]

    it "airs an underwriter at most twice a day" $ do
      let entries = planDay (input (showBreaks 9) [] [creative 5 1])
      length (concat (spotsPerBreak entries)) `shouldBe` underwritingDailyCap

    it "spreads the spots evenly over the show breaks" $ do
      let entries = planDay (input (showBreaks 8) [] [creative 5 1])
          positions = [ix | (ix, s) <- zip [0 :: Int ..] (spotsPerBreak entries), not (null s)]
      positions `shouldBe` [0, 4]

    it "gives scarce slots to the underwriter used least recently" $ do
      -- One break, two underwriters. Underwriter 2 was used earlier in the day.
      let cs = [usedAt 9 (creative 5 1), usedAt 8 (creative 7 2)]
          entries = planDay (input (showBreaks 1) [] cs)
      spotsPerBreak entries `shouldBe` [[BreakItems.Id 7]]

    it "puts a never-used underwriter ahead of one that was used" $ do
      let cs = [usedAt 8 (creative 5 1), creative 7 2]
          entries = planDay (input (showBreaks 1) [] cs)
      spotsPerBreak entries `shouldBe` [[BreakItems.Id 7]]

    it "alternates an underwriter's recordings" $ do
      let entries = planDay (input (showBreaks 4) [] [creative 5 1, creative 6 1])
      concat (spotsPerBreak entries) `shouldBe` map BreakItems.Id [5, 6]

    it "keeps the announcement when a long station ID leaves too little room" $ do
      -- A 73 second station ID and a 20 second announcement leave 27 seconds,
      -- which no 30 second PSA fits.
      let longId = StationIdCandidate (StationIds.Id 1) 73 Nothing
          i = (input (showBreaks 1) [psa 100 30] [creative 5 1]) {piStationIds = [longId]}
      itemsPerBreak (planDay i) `shouldBe` [[BreakItems.Id 5]]

    it "leaves out an announcement where the first step gave no spot" $ do
      -- One underwriter with two recordings gets two spots, in the first two
      -- of three show breaks. The third break has room, but gets no spot.
      let entries = planDay (input (showBreaks 3) [] [creative 5 1, creative 6 1])
      spotsPerBreak entries `shouldBe` [[BreakItems.Id 5], [BreakItems.Id 6], []]

    it "skips the automation break and gives the spot to the show break" $ do
      let automation = PlanBreak (UTCTime day 32400) AutomationBreak
          bs = automation : showBreaks 1
          entries = planDay (input bs [] [creative 5 1])
      spotsPerBreak entries `shouldBe` [[], [BreakItems.Id 5]]

  describe "PSAs" $ do
    it "takes the least recently used first" $ do
      let old = (psa 100 30) {bicLastUsed = Just (UTCTime day 0)}
          new = psa 101 30
          entries = planDay (input (showBreaks 1) [old, new] [])
      itemsPerBreak entries `shouldBe` [[BreakItems.Id 101, BreakItems.Id 100]]

    it "rotates so the next break starts with a different one" $ do
      -- Each PSA is 60 seconds, so only one fits after the station ID.
      let entries = planDay (input (showBreaks 2) [psa 100 60, psa 101 60] [])
      itemsPerBreak entries `shouldBe` [[BreakItems.Id 100], [BreakItems.Id 101]]

    it "skips a PSA that does not fit and takes a shorter one after it" $ do
      -- A 73 second station ID and the first 30 second PSA leave 17 seconds.
      -- The second 30 second PSA does not fit, so the 15 second PSA goes in.
      let longId = StationIdCandidate (StationIds.Id 1) 73 Nothing
          psas = [psa 100 30, psa 101 30, psa 102 15]
          i = (input (showBreaks 1) psas []) {piStationIds = [longId]}
      itemsPerBreak (planDay i) `shouldBe` [[BreakItems.Id 100, BreakItems.Id 102]]

    it "moves only the placed PSAs to the back of the rotation" $ do
      -- In break 1, PSA 101 does not fit and stays at the front. So break 2
      -- starts with it. Then PSA 100 does not fit, and PSA 102 goes in.
      let longId = StationIdCandidate (StationIds.Id 1) 73 Nothing
          psas = [psa 100 30, psa 101 30, psa 102 15]
          i = (input (showBreaks 2) psas []) {piStationIds = [longId]}
      itemsPerBreak (planDay i)
        `shouldBe` [ [BreakItems.Id 100, BreakItems.Id 102],
                     [BreakItems.Id 101, BreakItems.Id 102]
                   ]

    it "keeps each break within 120 seconds" $ do
      let psas = [psa i 30 | i <- [100 .. 110]]
          entries = planDay (input (showBreaks 2) psas [creative 5 1])
          secondsOf (StationIdRef _) = 10
          secondsOf (BreakItemRef i)
            | i == BreakItems.Id 5 = 20
            | otherwise = 30
          perBreak =
            [ sum [secondsOf (peRef e) | e <- entries, peBoundary e == b]
            | b <- map pbBoundary (showBreaks 2)
            ]
      perBreak `shouldSatisfy` all (<= breakSeconds)

  it "gives the same plan every time" $ do
    let i = input (showBreaks 9) [psa 100 30] [creative 5 1, creative 7 2]
    planDay i `shouldBe` planDay i
