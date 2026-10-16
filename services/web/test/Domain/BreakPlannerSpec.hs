-- | Tests for the pure daily break planner.
module Domain.BreakPlannerSpec where

--------------------------------------------------------------------------------

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Time (Day, UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Domain.BreakPlanner
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule (BreakKind (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Hedgehog (Gen, PropertyT, annotate, annotateShow, assert, failure, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

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
-- Generators

-- | A random day, with the shape of a real day.
genPlanInput :: Gen PlanInput
genPlanInput = do
  breaks <- genBreaks
  stationIds <- genStationIds
  underwriting <- genUnderwriting
  psas <- genPsas
  pure (PlanInput day breaks stationIds (underwriting <> psas))

-- | Up to 48 breaks, 30 minutes apart, in time order. Each break is a show
-- break or an automation break.
genBreaks :: Gen [PlanBreak]
genBreaks = do
  kinds <- Gen.list (Range.linear 0 48) (Gen.element [ShowBreak, AutomationBreak])
  pure (zipWith PlanBreak boundaries kinds)
  where
    boundaries = map pbBoundary (showBreaks 48)

-- | Up to 3 station IDs.
genStationIds :: Gen [StationIdCandidate]
genStationIds = do
  stationIdCount <- Gen.int64 (Range.linear 0 3)
  traverse genStationId [1 .. stationIdCount]

-- | One station ID. Most are 5 to 10 seconds. An old upload can be longer
-- than the whole break.
genStationId :: Int64 -> Gen StationIdCandidate
genStationId stationIdNumber = do
  seconds <-
    Gen.frequency
      [ (8, Gen.int64 (Range.linear 5 10)),
        (1, Gen.int64 (Range.linear 11 20)),
        (1, Gen.int64 (Range.linear 21 130))
      ]
  lastUsed <- genLastUsed
  pure
    StationIdCandidate
      { sicId = StationIds.Id stationIdNumber,
        sicSeconds = seconds,
        sicLastUsed = lastUsed
      }

-- | Up to 6 underwriters with 1 to 3 recordings each.
genUnderwriting :: Gen [BreakItemCandidate]
genUnderwriting = do
  underwriterCount <- Gen.int64 (Range.linear 0 6)
  recordings <- traverse genRecordings [1 .. underwriterCount]
  pure (concat recordings)

-- | The recordings of one underwriter.
--
-- Underwriter u owns the item ids from 10u + 1 to 10u + 3. Thus the ids are
-- unique, and they stay below the PSA ids.
genRecordings :: Int64 -> Gen [BreakItemCandidate]
genRecordings underwriterNumber = do
  recordingCount <- Gen.int64 (Range.linear 1 3)
  traverse genRecording [1 .. recordingCount]
  where
    genRecording recordingNumber = do
      seconds <- Gen.int64 (Range.linear 10 30)
      lastUsed <- genLastUsed
      pure
        BreakItemCandidate
          { bicId = BreakItems.Id (underwriterNumber * 10 + recordingNumber),
            bicCategory = BreakItems.Underwriting,
            bicUnderwriterId = Just (Underwriters.Id underwriterNumber),
            bicSeconds = seconds,
            bicLastUsed = lastUsed
          }

-- | Up to 10 PSAs, with ids from 100 up. Most are 10 to 30 seconds. Some old
-- uploads are up to 60 seconds.
genPsas :: Gen [BreakItemCandidate]
genPsas = do
  psaCount <- Gen.int64 (Range.linear 0 10)
  traverse genPsa [100 .. 99 + psaCount]
  where
    genPsa itemId = do
      seconds <-
        Gen.frequency
          [ (4, Gen.int64 (Range.linear 10 30)),
            (1, Gen.int64 (Range.linear 31 60))
          ]
      lastUsed <- genLastUsed
      pure
        BreakItemCandidate
          { bicId = BreakItems.Id itemId,
            bicCategory = BreakItems.Psa,
            bicUnderwriterId = Nothing,
            bicSeconds = seconds,
            bicLastUsed = lastUsed
          }

-- | Nothing, or a minute on the day before the test day.
genLastUsed :: Gen (Maybe UTCTime)
genLastUsed =
  Gen.maybe $ do
    minute <- Gen.integral (Range.linear 0 (1439 :: Integer))
    pure (UTCTime previousDay (secondsToDiffTime (minute * 60)))
  where
    previousDay = fromGregorian 2025 1 9

--------------------------------------------------------------------------------
-- Property helpers

-- | The kind of a planned entry. The order is the rule order.
data EntryKind
  = StationIdKind
  | UnderwritingKind
  | PsaKind
  deriving stock (Show, Eq, Ord)

-- | What the input says about a planned entry.
data EntryInfo = EntryInfo
  { eiKind :: EntryKind,
    eiSeconds :: Int64,
    -- | Nothing for every kind except underwriting.
    eiUnderwriter :: Maybe Underwriters.Id
  }
  deriving stock (Show)

-- | Find a planned entry in the input. Nothing if the input does not have it.
lookupEntry :: PlanInput -> EntryRef -> Maybe EntryInfo
lookupEntry planInput ref =
  case ref of
    StationIdRef stationIdId ->
      case Map.lookup stationIdId stationIdsById of
        Nothing -> Nothing
        Just s -> Just (EntryInfo StationIdKind (sicSeconds s) Nothing)
    BreakItemRef itemId ->
      case Map.lookup itemId itemsById of
        Nothing -> Nothing
        Just item -> Just (EntryInfo (kindOf item) (bicSeconds item) (bicUnderwriterId item))
  where
    stationIdsById = Map.fromList [(sicId s, s) | s <- piStationIds planInput]
    itemsById = Map.fromList [(bicId item, item) | item <- piBreakItems planInput]
    kindOf item =
      case bicCategory item of
        BreakItems.Underwriting -> UnderwritingKind
        BreakItems.Psa -> PsaKind

-- | Find a planned entry in the input. Fail the property if it is not there.
requireEntry :: PlanInput -> EntryRef -> PropertyT IO EntryInfo
requireEntry planInput ref =
  case lookupEntry planInput ref of
    Nothing -> do
      annotate ("The plan has an entry that is not in the input: " <> show ref)
      failure
    Just info -> pure info

-- | The entries of each planned break, in play order.
entriesByBreak :: [PlanEntry] -> [(UTCTime, [PlanEntry])]
entriesByBreak entries =
  Map.toList (Map.map (sortOn pePosition) grouped)
  where
    grouped = Map.fromListWith (<>) [(peBoundary entry, [entry]) | entry <- entries]

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

    it "spreads six spots from three underwriters evenly over 18 show breaks" $ do
      -- Three underwriters with two turns each give six spots. Spot i goes to
      -- show break (i * 18) div 6, so a spot lands on every third break. No
      -- underwriter has aired, so the lower underwriter id takes its turn first.
      let cs = [creative 5 1, creative 6 2, creative 7 3]
          entries = planDay (input (showBreaks 18) [] cs)
          placed =
            [ (ix, spot)
            | (ix, spots) <- zip [0 :: Int ..] (spotsPerBreak entries),
              spot <- spots
            ]
      placed `shouldBe` zip [0, 3, 6, 9, 12, 15] (map BreakItems.Id [5, 6, 7, 5, 6, 7])

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

  describe "invariants" $ do
    it "keeps the limits of every break" $ hedgehog $ do
      planInput <- forAll genPlanInput
      let breakKinds = Map.fromList [(pbBoundary b, pbKind b) | b <- piBreaks planInput]
      for_ (entriesByBreak (planDay planInput)) $ \(boundary, breakEntries) -> do
        annotateShow boundary
        infos <- traverse (requireEntry planInput . peRef) breakEntries
        let kinds = map eiKind infos
            countOf kind = length (filter (== kind) kinds)
            totalSeconds = sum (map eiSeconds infos)

        -- The positions count up from 0 with no gaps.
        map pePosition breakEntries === take (length breakEntries) [0 ..]

        assert (countOf StationIdKind <= 1)
        assert (countOf UnderwritingKind <= 1)
        assert (countOf PsaKind <= psasPerBreak)

        -- The play order is the rule order.
        kinds === sort kinds

        -- The station ID always plays. Each later item plays only if it fits
        -- in the time left. Thus a break can run longer than 'breakSeconds'
        -- only when the station ID is its one item.
        assert (totalSeconds <= breakSeconds || kinds == [StationIdKind])

        when (countOf UnderwritingKind > 0) $
          Map.lookup boundary breakKinds === Just ShowBreak

    it "keeps the daily underwriting limits" $ hedgehog $ do
      planInput <- forAll genPlanInput
      infos <- traverse (requireEntry planInput . peRef) (planDay planInput)
      let underwriterIds =
            [ underwriterId
            | info <- infos,
              eiKind info == UnderwritingKind,
              Just underwriterId <- [eiUnderwriter info]
            ]
          airsPerUnderwriter = Map.fromListWith (+) [(u, 1 :: Int) | u <- underwriterIds]
          underwritingCount = length (filter (\info -> eiKind info == UnderwritingKind) infos)
          showBreakCount = length [b | b <- piBreaks planInput, pbKind b == ShowBreak]
      annotateShow airsPerUnderwriter
      assert (all (<= underwritingDailyCap) (Map.elems airsPerUnderwriter))
      assert (underwritingCount <= showBreakCount)

    it "plans only items and breaks from the input" $ hedgehog $ do
      planInput <- forAll genPlanInput
      let inputBoundaries = map pbBoundary (piBreaks planInput)
      for_ (planDay planInput) $ \entry -> do
        annotateShow entry
        _ <- requireEntry planInput (peRef entry)
        assert (peBoundary entry `elem` inputBoundaries)

    it "gives the same plan every time" $ hedgehog $ do
      planInput <- forAll genPlanInput
      planDay planInput === planDay planInput

    it "opens every break with a station ID when the day has one" $ hedgehog $ do
      planInput <- forAll genPlanInput
      let openers =
            Map.fromList
              [(peBoundary entry, peRef entry) | entry <- planDay planInput, pePosition entry == 0]
          isStationId ref =
            case ref of
              StationIdRef _ -> True
              BreakItemRef _ -> False
      unless (null (piStationIds planInput)) $
        for_ (piBreaks planInput) $ \planBreak -> do
          annotateShow planBreak
          fmap isStationId (Map.lookup (pbBoundary planBreak) openers) === Just True
