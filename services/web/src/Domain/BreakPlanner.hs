-- | The daily break planner.
--
-- A pure function from one Pacific day's breaks and candidates to the stored
-- plan. The break endpoint reads the plan and chooses nothing, so a repeated
-- request for a break returns the same tracks.
--
-- Every break follows one template:
--
-- 1. A station ID, always
-- 2. Up to 'psasPerBreak' PSAs
-- 3. One underwriting announcement, at a show break only
--
-- Ephemeral filler covers any time left over. Liquidsoap plays it after the
-- break's tracks end.
module Domain.BreakPlanner
  ( -- * Input
    PlanBreak (..),
    StationIdCandidate (..),
    PsaCandidate (..),
    Creative (..),
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

-- | A PSA the planner can fill a break with.
data PsaCandidate = PsaCandidate
  { pcId :: BreakItems.Id,
    pcSeconds :: Int64,
    -- | The last planned boundary that used it. Nothing if never used.
    pcLastUsed :: Maybe UTCTime
  }
  deriving stock (Show, Eq)

-- | An underwriting announcement active on the day being planned.
data Creative = Creative
  { crId :: BreakItems.Id,
    crUnderwriterId :: Underwriters.Id,
    crSeconds :: Int64,
    -- | The last planned boundary that used it. Nothing if never used.
    crLastUsed :: Maybe UTCTime
  }
  deriving stock (Show, Eq)

-- | Everything the planner needs for one Pacific day.
data PlanInput = PlanInput
  { piDay :: Day,
    -- | In time order.
    piBreaks :: [PlanBreak],
    piStationIds :: [StationIdCandidate],
    piPsas :: [PsaCandidate],
    piCreatives :: [Creative]
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
-- The break template

-- | How long a break runs, in seconds.
breakSeconds :: Int64
breakSeconds = 120

-- | The most PSAs one break carries.
psasPerBreak :: Int
psasPerBreak = 2

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
stationIdMaxSeconds = 10

-- | The longest PSA an upload may carry.
psaMaxSeconds :: Int64
psaMaxSeconds = 30

-- | The longest underwriting announcement an upload may carry.
underwritingMaxSeconds :: Int64
underwritingMaxSeconds = 30

--------------------------------------------------------------------------------
-- Planning

-- | One break while the planner fills it.
data Slot = Slot
  { slBreak :: PlanBreak,
    slStationId :: Maybe StationIdCandidate,
    slUsed :: Int64,
    slPsas :: [BreakItems.Id],
    slSpot :: Maybe BreakItems.Id
  }

-- | Plan every break of one day.
--
-- The underwriting spot is placed before the PSAs, so the PSAs fill the time
-- it leaves. A station ID longer than the template allows then costs a PSA,
-- never the announcement.
planDay :: PlanInput -> [PlanEntry]
planDay input =
  concatMap toEntries $
    fillPsas (piPsas input) $
      placeUnderwriting (piCreatives input) $
        assignStationIds (piStationIds input) (piBreaks input)

-- | Give each break the least recently used station ID, rotating through them.
--
-- A station ID never used goes first. Ties go to the lower id.
assignStationIds :: [StationIdCandidate] -> [PlanBreak] -> [Slot]
assignStationIds candidates = go (sortOn key candidates)
  where
    key c = (isJust (sicLastUsed c), sicLastUsed c, sicId c)
    go _ [] = []
    go [] (b : bs) = Slot b Nothing 0 [] Nothing : go [] bs
    go (c : cs) (b : bs) = Slot b (Just c) (sicSeconds c) [] Nothing : go (cs <> [c]) bs

-- | Place the day's underwriting, one spot per show break at most.
--
-- Underwriters take turns, least recently used first, up to
-- 'underwritingDailyCap' rounds. When the turns outnumber the show breaks, the
-- underwriters used least recently keep their turns, so the share evens out
-- over the days. The spots spread evenly over the day's show breaks. Each
-- underwriter's recordings alternate, least recently used first.
placeUnderwriting :: [Creative] -> [Slot] -> [Slot]
placeUnderwriting creatives slots =
  foldl place slots (zip positions turns)
  where
    showIdx = [ix | (ix, s) <- zip [0 :: Int ..] slots, pbKind (slBreak s) == ShowBreak]
    breaks = length showIdx

    -- Each underwriter's recordings, least recently used first.
    byUnderwriter =
      Map.map (sortOn lastUsedKey) $
        Map.fromListWith (<>) [(crUnderwriterId c, [c]) | c <- creatives]
    underwriterKey (uw, cs) = (lastAired cs, uw)
    lastAired cs = maximum (map crLastUsed cs)
    ordered = sortOn underwriterKey (Map.toList byUnderwriter)

    -- Round r gives each underwriter its r-th recording, cycling.
    allTurns =
      [ cs !! (r `mod` length cs)
      | r <- [0 .. underwritingDailyCap - 1],
        (_, cs) <- ordered
      ]
    turns = take breaks allTurns
    spots = length turns
    positions =
      [showIdx !! ((i * breaks) `div` spots) | i <- [0 .. spots - 1]]

    place ss (ix, c) = updateAt ix (addSpot c) ss
    addSpot c s
      | slUsed s + crSeconds c <= breakSeconds =
          s {slUsed = slUsed s + crSeconds c, slSpot = Just (crId c)}
      | otherwise = s

-- | Sort key that puts never-used first, then the oldest use.
lastUsedKey :: Creative -> (Bool, Maybe UTCTime, BreakItems.Id)
lastUsedKey c = (isJust (crLastUsed c), crLastUsed c, crId c)

-- | Fill each break with up to 'psasPerBreak' PSAs, least recently used first.
--
-- A PSA that does not fit in the time left is skipped, and the walk continues.
-- A PSA placed in a break moves to the back of the rotation.
fillPsas :: [PsaCandidate] -> [Slot] -> [Slot]
fillPsas candidates = go (sortOn key candidates)
  where
    key p = (isJust (pcLastUsed p), pcLastUsed p, pcId p)
    go _ [] = []
    go rotation (s : ss) =
      let s' = foldl step s rotation
          placed = slPsas s'
          rotation' =
            [p | p <- rotation, pcId p `notElem` placed]
              <> [p | p <- rotation, pcId p `elem` placed]
       in s' : go rotation' ss
    step s p
      | length (slPsas s) < psasPerBreak && slUsed s + pcSeconds p <= breakSeconds =
          s {slUsed = slUsed s + pcSeconds p, slPsas = slPsas s <> [pcId p]}
      | otherwise = s

-- | Turn a filled break into its entries: station ID, PSAs, then the spot.
toEntries :: Slot -> [PlanEntry]
toEntries s =
  zipWith (PlanEntry (pbBoundary (slBreak s))) [0 ..] $
    maybe [] (\sid -> [StationIdRef (sicId sid)]) (slStationId s)
      <> map BreakItemRef (slPsas s)
      <> maybe [] (\c -> [BreakItemRef c]) (slSpot s)

-- | Replace the element at an index.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt ix f xs = [if j == ix then f x else x | (j, x) <- zip [0 ..] xs]
