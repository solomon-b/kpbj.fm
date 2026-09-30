-- | The daily break planner.
--
-- A pure function from one Pacific day's breaks and candidates to the stored
-- plan. The break endpoint reads the plan and chooses nothing, so a repeated
-- request for a break returns the same tracks.
--
-- Every break opens with a station ID. A show break then carries underwriting
-- announcements, then PSAs. An automation break carries PSAs only.
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

    -- * Planning
    breakSeconds,
    dailyQuota,
    owedInMonth,
    underwriterOffset,
    planDay,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time (Day, UTCTime, diffDays, fromGregorian, gregorianMonthLength, toGregorian)
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
    -- | First air date of the order.
    crStartsOn :: Day,
    -- | Last air date of the order. Nothing runs until stopped.
    crEndsOn :: Maybe Day,
    crSpotsPerMonth :: Int64,
    -- | Airings this Pacific month, before the day being planned.
    crDeliveredThisMonth :: Int64
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
-- Planning

-- | How long a break runs, in seconds.
breakSeconds :: Int64
breakSeconds = 120

-- | Spots to place today.
--
-- This is the spots still owed this month over the days the order has left in
-- the month, today included, rounded up. The remaining count comes from what
-- aired, so a bad day is made up over the days after it.
dailyQuota :: Day -> Creative -> Int64
dailyQuota today c
  | remaining <= 0 = 0
  | otherwise = (remaining + daysLeft - 1) `div` daysLeft
  where
    owed = owedInMonth today (crStartsOn c) (crEndsOn c) (crSpotsPerMonth c)
    remaining = owed - crDeliveredThisMonth c
    (_, monthLast) = monthBounds today
    lastDay = maybe monthLast (min monthLast) (crEndsOn c)
    daysLeft = max 1 (fromIntegral (diffDays lastDay today + 1))

-- | Spots an order owes in the month that contains the given day.
--
-- An order that runs part of the month owes that part of its monthly spots,
-- rounded up, so an underwriter never gets less than it paid for. An order
-- that runs the whole month owes all of them.
owedInMonth ::
  -- | Any day of the month
  Day ->
  -- | First air date of the order
  Day ->
  -- | Last air date of the order
  Maybe Day ->
  -- | Spots per month
  Int64 ->
  Int64
owedInMonth dayInMonth startsOn endsOn spotsPerMonth
  | activeDays <= 0 = 0
  | otherwise = (spotsPerMonth * activeDays + daysInMonth - 1) `div` daysInMonth
  where
    (monthFirst, monthLast) = monthBounds dayInMonth
    daysInMonth = fromIntegral (diffDays monthLast monthFirst + 1)
    activeFirst = max monthFirst startsOn
    activeLast = maybe monthLast (min monthLast) endsOn
    activeDays = fromIntegral (diffDays activeLast activeFirst + 1)

-- | The first and last day of the month that contains the given day.
monthBounds :: Day -> (Day, Day)
monthBounds d =
  let (y, m, _) = toGregorian d
   in (fromGregorian y m 1, fromGregorian y m (gregorianMonthLength y m))

-- | A fixed value from 0 up to but not including 1, for each underwriter.
--
-- It shifts where an underwriter's spots fall in the day, so two underwriters
-- with equal totals do not land in the same breaks. It is plain arithmetic, so
-- it gives the same value on every run.
underwriterOffset :: Underwriters.Id -> Double
underwriterOffset (Underwriters.Id n) = fromIntegral ((n * 2654435761) `mod` 1000) / 1000

-- | One break while the planner fills it.
data Slot = Slot
  { slBreak :: PlanBreak,
    slStationId :: Maybe StationIdCandidate,
    slUsed :: Int64,
    slItems :: [BreakItems.Id],
    slUnderwriters :: [Underwriters.Id]
  }

-- | Plan every break of one day.
planDay :: PlanInput -> [PlanEntry]
planDay input =
  concatMap toEntries $
    fillPsas (piPsas input) $
      placeUnderwriting input $
        assignStationIds (piStationIds input) (piBreaks input)

-- | Place each underwriter's spots for the day in the show breaks.
--
-- Underwriters go in id order. Each underwriter's spots spread evenly across
-- the day's show breaks, shifted by its offset. A break never carries two spots
-- from one underwriter. A spot with no room moves to the next later show break
-- with room, and drops if none has room. The next day's quota makes it up.
placeUnderwriting :: PlanInput -> [Slot] -> [Slot]
placeUnderwriting input slots0 =
  foldl placeUnderwriter slots0 (Map.toAscList byUnderwriter)
  where
    byUnderwriter :: Map Underwriters.Id [Creative]
    byUnderwriter =
      Map.fromListWith (flip (<>)) [(crUnderwriterId c, [c]) | c <- piCreatives input]

    placeUnderwriter :: [Slot] -> (Underwriters.Id, [Creative]) -> [Slot]
    placeUnderwriter slots (uw, cs)
      | n == 0 = slots
      | otherwise = foldl (placeOne uw showIdx) slots (zip targets spots)
      where
        showIdx = [ix | (ix, s) <- zip [0 :: Int ..] slots, pbKind (slBreak s) == ShowBreak]
        k = length showIdx
        spots = alternate [(c, dailyQuota (piDay input) c) | c <- cs]
        n = min k (length spots)
        targets =
          [ floor ((fromIntegral i + underwriterOffset uw) * fromIntegral k / fromIntegral n :: Double)
          | i <- [0 .. n - 1]
          ]

    placeOne :: Underwriters.Id -> [Int] -> [Slot] -> (Int, Creative) -> [Slot]
    placeOne uw showIdx slots (target, c) =
      case [ix | ix <- drop target showIdx, fits (slots !! ix)] of
        [] -> slots
        (ix : _) -> updateAt ix addSpot slots
      where
        fits s =
          uw `notElem` slUnderwriters s
            && slUsed s + crSeconds c <= breakSeconds
        addSpot s =
          s
            { slUsed = slUsed s + crSeconds c,
              slItems = slItems s <> [crId c],
              slUnderwriters = uw : slUnderwriters s
            }

-- | Turn a filled break into its entries, station ID first.
toEntries :: Slot -> [PlanEntry]
toEntries s =
  zipWith (PlanEntry (pbBoundary (slBreak s))) [0 ..] $
    maybe [] (\sid -> [StationIdRef (sicId sid)]) (slStationId s)
      <> map BreakItemRef (slItems s)

-- | Give each break the least recently used station ID, rotating through them.
--
-- A station ID never used goes first. Ties go to the lower id.
assignStationIds :: [StationIdCandidate] -> [PlanBreak] -> [Slot]
assignStationIds candidates = go (sortOn key candidates)
  where
    key c = (isJust (sicLastUsed c), sicLastUsed c, sicId c)
    go _ [] = []
    go [] (b : bs) = Slot b Nothing 0 [] [] : go [] bs
    go (c : cs) (b : bs) = Slot b (Just c) (sicSeconds c) [] [] : go (cs <> [c]) bs

-- | One underwriter's spots for today, with its creatives alternating.
--
-- The creative with fewer airings this month goes first, then the lower id.
alternate :: [(Creative, Int64)] -> [Creative]
alternate withQuota = go (sortOn (\(c, _) -> (crDeliveredThisMonth c, crId c)) withQuota)
  where
    go xs
      | null live = []
      | otherwise = map fst live <> go [(c, q - 1) | (c, q) <- live]
      where
        live = [(c, q) | (c, q) <- xs, q > 0]

-- | After underwriting, fill each break with PSAs, least recently used first.
--
-- A PSA that does not fit is skipped, and the walk continues. A PSA placed in a
-- break moves to the back of the rotation.
fillPsas :: [PsaCandidate] -> [Slot] -> [Slot]
fillPsas candidates = go (sortOn key candidates)
  where
    key p = (isJust (pcLastUsed p), pcLastUsed p, pcId p)
    go _ [] = []
    go rotation (s : ss) =
      let (s', placed) = foldl step (s, []) rotation
          rotation' =
            [p | p <- rotation, pcId p `notElem` placed]
              <> [p | p <- rotation, pcId p `elem` placed]
       in s' : go rotation' ss
    step (s, placed) p
      | slUsed s + pcSeconds p <= breakSeconds =
          ( s {slUsed = slUsed s + pcSeconds p, slItems = slItems s <> [pcId p]},
            placed <> [pcId p]
          )
      | otherwise = (s, placed)

-- | Replace the element at an index.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt ix f xs = [if j == ix then f x else x | (j, x) <- zip [0 ..] xs]
