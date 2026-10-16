-- | The daily break planner.
--
-- A pure function from one Pacific day's breaks and candidates to the stored
-- plan. The break endpoint reads the plan and chooses nothing, so a repeated
-- request for a break returns the same tracks.
--
-- One list of rules, 'rules', controls every break. Each rule names a
-- category, the most items of that category in one break, the longest upload
-- for it, and a test that allows or refuses an item in a break. The order of
-- the list is the priority, and it is also the play order in a break:
--
-- 1. One station ID, in every break
-- 2. One underwriting announcement, in a show break only
-- 3. Up to 'psasPerBreak' PSAs, in every break
--
-- The plan is made in two steps:
--
-- 1. 'placeUnderwriting' gives the day's underwriting spots to show breaks.
--    It works on the whole day, because the spots must spread evenly.
-- 2. 'fillBreaks' fills each break in time order. It walks the rules in
--    priority order and takes the items each rule allows.
--
-- Ephemeral filler covers any time left over. Liquidsoap plays it after the
-- break's tracks end.
module Domain.BreakPlanner
  ( -- * Input
    PlanBreak (..),
    StationIdCandidate (..),
    BreakItemCandidate (..),
    PlanInput (..),

    -- * Output
    EntryRef (..),
    PlanEntry (..),

    -- * The break template
    breakSeconds,
    psasPerBreak,
    underwritingDailyCap,
    stationIdMaxSeconds,
    psaMaxSeconds,
    underwritingMaxSeconds,

    -- * Planning
    planDay,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time (Day, UTCTime)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule (BreakKind (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.Underwriters qualified as Underwriters

--------------------------------------------------------------------------------
-- Input

-- | One break of the day, identified by its boundary.
data PlanBreak = PlanBreak
  { pbBoundary :: UTCTime,
    pbKind :: BreakKind
  }
  deriving stock (Show, Eq)

-- | A station ID the planner can open a break with.
data StationIdCandidate = StationIdCandidate
  { sicId :: StationIds.Id,
    sicSeconds :: Int64,
    -- | The last planned boundary that used it. Nothing if never used.
    sicLastUsed :: Maybe UTCTime
  }
  deriving stock (Show, Eq)

-- | A break item active on the day being planned.
data BreakItemCandidate = BreakItemCandidate
  { bicId :: BreakItems.Id,
    bicCategory :: BreakItems.Category,
    -- | The underwriter an underwriting announcement acknowledges. Nothing for
    -- every other category.
    bicUnderwriterId :: Maybe Underwriters.Id,
    bicSeconds :: Int64,
    -- | The last planned boundary that used it. Nothing if never used.
    bicLastUsed :: Maybe UTCTime
  }
  deriving stock (Show, Eq)

-- | Everything the planner needs for one Pacific day.
data PlanInput = PlanInput
  { piDay :: Day,
    -- | In time order.
    piBreaks :: [PlanBreak],
    piStationIds :: [StationIdCandidate],
    piBreakItems :: [BreakItemCandidate]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Output

-- | What a planned track plays.
data EntryRef
  = StationIdRef StationIds.Id
  | BreakItemRef BreakItems.Id
  deriving stock (Show, Eq)

-- | One track of one break, in play order.
data PlanEntry = PlanEntry
  { peBoundary :: UTCTime,
    pePosition :: Int64,
    peRef :: EntryRef
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- The rules

-- | The kind of item a rule places.
--
-- Station IDs come from their own table, so they get their own category. Each
-- break item category gets one rotation.
data Category
  = StationIdCategory
  | BreakItemCategory BreakItems.Category
  deriving stock (Show, Eq, Ord)

-- | One item that a rule can place, as the fill loop sees it.
data Candidate = Candidate
  { cdRef :: EntryRef,
    cdSeconds :: Int64
  }
  deriving stock (Show, Eq)

-- | A break as the eligibility tests see it.
data RuleBreak = RuleBreak
  { rbBreak :: PlanBreak,
    -- | The underwriting spot that step 1 gave to this break, if any.
    rbSpot :: Maybe BreakItems.Id
  }

-- | How the fill loop places one category in a break.
data Rule = Rule
  { ruleCategory :: Category,
    -- | The most items of this category in one break.
    ruleMaxCount :: Int,
    -- | The longest item an upload may carry.
    --
    -- The fill loop does not use this value. Items saved before the limit
    -- can be longer, and the time left in the break limits them.
    ruleMaxSeconds :: Int64,
    -- | True when an allowed item goes in even if the break has too little
    -- time left for it.
    ruleAlwaysFits :: Bool,
    -- | True when the item may play in the break.
    ruleAllows :: RuleBreak -> Candidate -> Bool
  }

-- | Every rule, in priority order. This is also the play order in a break.
rules :: [Rule]
rules = [stationIdRule, underwritingRule, psaRule]

-- | One station ID opens every break.
--
-- An old station ID can be longer than 'breakSeconds'. It still plays, and the
-- rules after it get the time that is left, if any.
stationIdRule :: Rule
stationIdRule =
  Rule
    { ruleCategory = StationIdCategory,
      ruleMaxCount = 1,
      ruleMaxSeconds = 10,
      ruleAlwaysFits = True,
      ruleAllows = allowEverywhere
    }

-- | One underwriting announcement, in a show break only.
--
-- Only the spot that 'placeUnderwriting' gave to the break can play. The rule
-- comes before the PSAs, so a long station ID costs a PSA and never the
-- announcement.
underwritingRule :: Rule
underwritingRule =
  Rule
    { ruleCategory = BreakItemCategory BreakItems.Underwriting,
      ruleMaxCount = 1,
      ruleMaxSeconds = 30,
      ruleAlwaysFits = False,
      ruleAllows = allowAssignedSpot
    }

-- | Up to two PSAs, in every break.
psaRule :: Rule
psaRule =
  Rule
    { ruleCategory = BreakItemCategory BreakItems.Psa,
      ruleMaxCount = 2,
      ruleMaxSeconds = 30,
      ruleAlwaysFits = False,
      ruleAllows = allowEverywhere
    }

-- | Allow every item in every break.
allowEverywhere :: RuleBreak -> Candidate -> Bool
allowEverywhere _ _ = True

-- | Allow only the underwriting spot that step 1 gave to this show break.
allowAssignedSpot :: RuleBreak -> Candidate -> Bool
allowAssignedSpot ruleBreak candidate =
  case rbSpot ruleBreak of
    Nothing -> False
    Just spotId ->
      pbKind (rbBreak ruleBreak) == ShowBreak
        && cdRef candidate == BreakItemRef spotId

--------------------------------------------------------------------------------
-- The break template

-- | How long a break runs, in seconds.
breakSeconds :: Int64
breakSeconds = 120

-- | The most PSAs one break carries.
psasPerBreak :: Int
psasPerBreak = ruleMaxCount psaRule

-- | The most announcements one underwriter airs in a day, across all of its
-- recordings.
--
-- Every underwriter gets the same share. Once all of them reach the cap, the
-- remaining show breaks carry no underwriting.
underwritingDailyCap :: Int
underwritingDailyCap = 2

-- | The longest station ID an upload may carry.
--
-- With 'psaMaxSeconds' and 'underwritingMaxSeconds' this leaves 20 seconds for
-- a show promo, so a full break always fits in 'breakSeconds'.
stationIdMaxSeconds :: Int64
stationIdMaxSeconds = ruleMaxSeconds stationIdRule

-- | The longest PSA an upload may carry.
psaMaxSeconds :: Int64
psaMaxSeconds = ruleMaxSeconds psaRule

-- | The longest underwriting announcement an upload may carry.
underwritingMaxSeconds :: Int64
underwritingMaxSeconds = ruleMaxSeconds underwritingRule

--------------------------------------------------------------------------------
-- Planning

-- | Plan every break of one day.
--
-- Step 1 gives the underwriting spots to show breaks. Step 2 fills each break
-- from the rules.
planDay :: PlanInput -> [PlanEntry]
planDay input =
  fillBreaks spots startRotations (piBreaks input)
  where
    spots = placeUnderwriting (piBreakItems input) (piBreaks input)
    startRotations = initialRotations (piStationIds input) (piBreakItems input)

-- | The first rotation of each category, least recently used first.
--
-- An item never used goes first. Ties go to the lower id.
initialRotations :: [StationIdCandidate] -> [BreakItemCandidate] -> Map Category [Candidate]
initialRotations stationIds items =
  Map.fromList (stationIdRotation : breakItemRotations)
  where
    stationIdRotation =
      (StationIdCategory, map stationIdToCandidate (sortOn stationIdKey stationIds))

    breakItemRotations =
      [ (BreakItemCategory category, map breakItemToCandidate (sortOn breakItemKey inCategory))
      | category <- [minBound .. maxBound],
        let inCategory = filter (\item -> bicCategory item == category) items
      ]

    stationIdToCandidate s = Candidate (StationIdRef (sicId s)) (sicSeconds s)
    breakItemToCandidate item = Candidate (BreakItemRef (bicId item)) (bicSeconds item)

-- | Sort key that puts a never-used station ID first, then the oldest use.
stationIdKey :: StationIdCandidate -> (Bool, Maybe UTCTime, StationIds.Id)
stationIdKey s = (isJust (sicLastUsed s), sicLastUsed s, sicId s)

-- | Sort key that puts a never-used break item first, then the oldest use.
breakItemKey :: BreakItemCandidate -> (Bool, Maybe UTCTime, BreakItems.Id)
breakItemKey item = (isJust (bicLastUsed item), bicLastUsed item, bicId item)

-- | Step 1. Give the day's underwriting spots to show breaks, one per break at
-- most.
--
-- Underwriters take turns, least recently used first, up to
-- 'underwritingDailyCap' rounds. When the turns outnumber the show breaks, the
-- underwriters used least recently keep their turns, so the share evens out
-- over the days. The spots spread evenly over the day's show breaks. Each
-- underwriter's recordings alternate, least recently used first.
--
-- The result maps a break boundary to its spot. The fill loop checks that the
-- spot fits.
placeUnderwriting :: [BreakItemCandidate] -> [PlanBreak] -> Map UTCTime BreakItems.Id
placeUnderwriting items breaks =
  Map.fromList (zip positions (map bicId turns))
  where
    showBoundaries = [pbBoundary b | b <- breaks, pbKind b == ShowBreak]
    showBreakCount = length showBoundaries

    -- Each underwriter's recordings, least recently used first.
    byUnderwriter =
      Map.map (sortOn breakItemKey) $
        Map.fromListWith (<>) (concatMap withUnderwriter items)
    withUnderwriter item =
      case (bicCategory item, bicUnderwriterId item) of
        (BreakItems.Underwriting, Just underwriterId) -> [(underwriterId, [item])]
        _ -> []
    underwriterKey (underwriterId, recordings) = (lastAired recordings, underwriterId)
    lastAired recordings = maximum (map bicLastUsed recordings)
    ordered = sortOn underwriterKey (Map.toList byUnderwriter)

    -- Round r gives each underwriter its r-th recording, cycling.
    allTurns =
      [ recordings !! (r `mod` length recordings)
      | r <- [0 .. underwritingDailyCap - 1],
        (_, recordings) <- ordered
      ]
    turns = take showBreakCount allTurns
    spotCount = length turns
    positions =
      [ showBoundaries !! ((i * showBreakCount) `div` spotCount)
      | i <- [0 .. spotCount - 1]
      ]

-- | Step 2. Fill each break in time order.
--
-- Each category keeps its own rotation from one break to the next.
fillBreaks :: Map UTCTime BreakItems.Id -> Map Category [Candidate] -> [PlanBreak] -> [PlanEntry]
fillBreaks _ _ [] = []
fillBreaks spots rotations (planBreak : laterBreaks) =
  toEntries planBreak placed <> fillBreaks spots nextRotations laterBreaks
  where
    ruleBreak = RuleBreak planBreak (Map.lookup (pbBoundary planBreak) spots)
    filled = foldl (applyRule ruleBreak) (Filling 0 [] rotations) rules
    placed = flPlaced filled
    nextRotations = flRotations filled

-- | One break while the fill loop fills it.
data Filling = Filling
  { -- | The seconds that the placed items take.
    flUsed :: Int64,
    -- | The placed items, in play order.
    flPlaced :: [Candidate],
    flRotations :: Map Category [Candidate]
  }

-- | Place the items that one rule takes, and move them to the back of their
-- rotation.
applyRule :: RuleBreak -> Filling -> Rule -> Filling
applyRule ruleBreak filling rule =
  Filling
    { flUsed = flUsed filling + sum (map cdSeconds taken),
      flPlaced = flPlaced filling <> taken,
      flRotations = Map.insert category nextRotation (flRotations filling)
    }
  where
    category = ruleCategory rule
    rotation = Map.findWithDefault [] category (flRotations filling)
    taken = takeByRule ruleBreak rule (flUsed filling) rotation
    takenRefs = map cdRef taken
    nextRotation =
      [c | c <- rotation, cdRef c `notElem` takenRefs] <> taken

-- | Walk a rotation in order and take each item the rule allows and that fits,
-- up to the rule's max count.
--
-- An item that does not fit in the time left is skipped, and the walk
-- continues.
takeByRule ::
  RuleBreak ->
  Rule ->
  -- | Seconds already used in the break
  Int64 ->
  [Candidate] ->
  [Candidate]
takeByRule ruleBreak rule startUsed = go startUsed 0
  where
    go _ _ [] = []
    go used count (candidate : rest)
      | count >= ruleMaxCount rule = []
      | allowed && fits = candidate : go (used + cdSeconds candidate) (count + 1) rest
      | otherwise = go used count rest
      where
        allowed = ruleAllows rule ruleBreak candidate
        fits = ruleAlwaysFits rule || used + cdSeconds candidate <= breakSeconds

-- | Turn a filled break into its entries, in play order.
toEntries :: PlanBreak -> [Candidate] -> [PlanEntry]
toEntries planBreak placed =
  zipWith (PlanEntry (pbBoundary planBreak)) [0 ..] (map cdRef placed)
