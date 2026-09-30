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

-- | January 10, 2025. January has 31 days, so 22 days are left, the 10th included.
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

-- | A 20 second creative that runs the whole month.
creative ::
  -- | Break item id
  Int64 ->
  -- | Underwriter id
  Int64 ->
  -- | Spots per month
  Int64 ->
  -- | Delivered this month
  Int64 ->
  Creative
creative itemId uw =
  Creative (BreakItems.Id itemId) (Underwriters.Id uw) 20 (fromGregorian 2025 1 1) Nothing

input :: [PlanBreak] -> [PsaCandidate] -> [Creative] -> PlanInput
input bs = PlanInput day bs [stationId]

-- | The break items in each break, in break order and play order.
itemsPerBreak :: [PlanEntry] -> [[BreakItems.Id]]
itemsPerBreak entries =
  [ [i | PlanEntry b' _ (BreakItemRef i) <- sortOn pePosition entries, b' == b]
  | b <- nub (map peBoundary entries)
  ]

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.BreakPlanner" $ do
  describe "dailyQuota" $ do
    it "spreads the remaining spots over the days left" $
      -- 30 spots, 8 delivered, 22 days left: 22 / 22 = 1.
      dailyQuota day (creative 1 1 30 8) `shouldBe` 1
    it "rounds up" $
      -- 60 spots, none delivered, 22 days left: 60 / 22 is about 2.7, so 3.
      dailyQuota day (creative 1 1 60 0) `shouldBe` 3
    it "is zero once the month is delivered" $
      dailyQuota day (creative 1 1 30 30) `shouldBe` 0
    it "is zero when the month is over delivered" $
      dailyQuota day (creative 1 1 30 31) `shouldBe` 0
    it "puts everything left on the last day of the month" $
      dailyQuota (fromGregorian 2025 1 31) (creative 1 1 30 25) `shouldBe` 5
    it "spreads a partial month's spots over the days the order runs" $ do
      -- Starts on the 10th, so it owes 30 × 22 / 31, about 21.3, so 22.
      -- 22 spots over 22 days left: 1 a day.
      let c = (creative 1 1 30 0) {crStartsOn = fromGregorian 2025 1 10}
      dailyQuota day c `shouldBe` 1
    it "delivers everything owed by an order's last day" $ do
      -- Ends on the 15th, so it owes 30 × 15 / 31, about 14.5, so 15.
      -- 15 spots over the 6 days from the 10th to the 15th: 2.5, so 3.
      let c = (creative 1 1 30 0) {crEndsOn = Just (fromGregorian 2025 1 15)}
      dailyQuota day c `shouldBe` 3

  describe "owedInMonth" $ do
    it "owes the full count for a whole month" $
      owedInMonth day (fromGregorian 2025 1 1) Nothing 30 `shouldBe` 30
    it "rounds a partial month up" $
      -- 10 days of 31: 30 × 10 / 31 is about 9.7, so 10.
      owedInMonth day (fromGregorian 2025 1 22) Nothing 30 `shouldBe` 10
    it "owes nothing in a month the order does not reach" $
      owedInMonth day (fromGregorian 2025 2 1) Nothing 30 `shouldBe` 0
    it "counts an order that starts and ends inside the month" $
      -- The 5th to the 7th is 3 days of 31: 31 × 3 / 31 = 3.
      owedInMonth day (fromGregorian 2025 1 5) (Just (fromGregorian 2025 1 7)) 31 `shouldBe` 3

  describe "planDay" $ do
    it "opens every break with a station ID at position 0" $ do
      let entries = planDay (input (showBreaks 3) [] [])
      [peRef e | e <- entries, pePosition e == 0]
        `shouldBe` replicate 3 (StationIdRef (StationIds.Id 1))

    it "never puts underwriting in an automation break" $ do
      let bs = [PlanBreak (UTCTime day 0) AutomationBreak]
          entries = planDay (input bs [] [creative 5 1 3000 0])
      [i | PlanEntry _ _ (BreakItemRef i) <- entries] `shouldBe` []

    it "places the daily quota once per break, spread across the day" $ do
      -- 66 spots, none delivered, 22 days left: a quota of 3, over 9 breaks.
      let entries = planDay (input (showBreaks 9) [] [creative 5 1 66 0])
          placed = map length (itemsPerBreak entries)
      sum placed `shouldBe` 3
      maximum placed `shouldBe` 1

    it "leaves at least two breaks between spots of one underwriter" $ do
      let entries = planDay (input (showBreaks 9) [] [creative 5 1 66 0])
          positions = [ix | (ix, is) <- zip [0 :: Int ..] (itemsPerBreak entries), not (null is)]
      zipWith (-) (drop 1 positions) positions `shouldSatisfy` all (>= 3)

    it "never puts two spots from one underwriter in one break" $ do
      -- Two creatives of underwriter 1, a total quota of 6, over 3 breaks.
      let cs = [creative 5 1 66 0, creative 6 1 66 0]
          entries = planDay (input (showBreaks 3) [] cs)
      map length (itemsPerBreak entries) `shouldSatisfy` all (<= 1)

    it "caps an underwriter at one spot per show break" $ do
      let cs = [creative 5 1 66 0, creative 6 1 66 0]
          entries = planDay (input (showBreaks 3) [] cs)
      sum (map length (itemsPerBreak entries)) `shouldBe` 3

    it "alternates an underwriter's creatives" $ do
      -- 44 spots each, none delivered: a quota of 2 each.
      let cs = [creative 5 1 44 0, creative 6 1 44 0]
          entries = planDay (input (showBreaks 8) [] cs)
      concat (itemsPerBreak entries)
        `shouldBe` map BreakItems.Id [5, 6, 5, 6]

    it "keeps each break within 120 seconds" $ do
      let psas = [PsaCandidate (BreakItems.Id i) 30 Nothing | i <- [100 .. 110]]
          entries = planDay (input (showBreaks 2) psas [creative 5 1 44 0])
          secondsOf (StationIdRef _) = 10
          secondsOf (BreakItemRef i)
            | i == BreakItems.Id 5 = 20
            | otherwise = 30
          perBreak =
            [ sum [secondsOf (peRef e) | e <- entries, peBoundary e == b]
            | b <- map pbBoundary (showBreaks 2)
            ]
      perBreak `shouldSatisfy` all (<= breakSeconds)

    it "fills the rest of a break with PSAs, least recently used first" $ do
      let old = PsaCandidate (BreakItems.Id 100) 30 (Just (UTCTime day 0))
          new = PsaCandidate (BreakItems.Id 101) 30 Nothing
          entries = planDay (input (showBreaks 1) [old, new] [])
      map peRef (sortOn pePosition entries)
        `shouldBe` [ StationIdRef (StationIds.Id 1),
                     BreakItemRef (BreakItems.Id 101),
                     BreakItemRef (BreakItems.Id 100)
                   ]

    it "rotates PSAs so the next break starts with a different one" $ do
      -- Each PSA is 60 seconds, so only one fits after the station ID.
      let psas = [PsaCandidate (BreakItems.Id i) 60 Nothing | i <- [100, 101]]
          entries = planDay (input (showBreaks 2) psas [])
      itemsPerBreak entries `shouldBe` [[BreakItems.Id 100], [BreakItems.Id 101]]

    it "places underwriting before PSAs" $ do
      -- One break. A 100 second PSA would fill it, but underwriting goes first.
      let psas = [PsaCandidate (BreakItems.Id 100) 100 Nothing]
          entries = planDay (input (showBreaks 1) psas [creative 5 1 3100 0])
      itemsPerBreak entries `shouldBe` [[BreakItems.Id 5]]

    it "offsets two underwriters with equal totals into different breaks" $ do
      -- A quota of 1 each, over 10 breaks.
      let entries = planDay (input (showBreaks 10) [] [creative 5 1 22 0, creative 7 2 22 0])
      length (filter (not . null) (itemsPerBreak entries)) `shouldBe` 2

    it "gives each break a station ID on a 46 break day" $ do
      let entries = planDay (input (showBreaks 46) [] [])
      length [() | PlanEntry _ 0 (StationIdRef _) <- entries] `shouldBe` 46

    it "gives the same plan every time" $ do
      let i = input (showBreaks 9) [] [creative 5 1 66 0, creative 7 2 66 0]
      planDay i `shouldBe` planDay i
