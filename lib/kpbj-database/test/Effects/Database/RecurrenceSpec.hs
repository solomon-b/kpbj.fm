{-# LANGUAGE QuasiQuotes #-}

-- | Tests for the two SQL functions that hold the schedule recurrence rule.
--
-- @recurrence_airs_on(day_num, weeks, date)@ answers one question: does a show
-- that airs on a weekday, in given weeks of the month, air on a date?
-- @day_of_week_num@ maps the @day_of_week@ enum to the number @EXTRACT(DOW ...)@
-- returns.
--
-- Ten queries across ShowSchedule.hs and Episodes.hs call them, so a defect here
-- reaches the public schedule, the upload dropdown, the conflict check, the
-- missing-episode reports, and the stream. Before this rule had one definition
-- it was written out at each of those sites, and the copies drifted.
module Effects.Database.RecurrenceSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), fromGregorian)
import Data.Time qualified as Time
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowSchedule (dayOfWeekNumber)
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Gen.Tables.ShowSchedule (weekOfMonth)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $ do
    describe "recurrence_airs_on" $ do
      it "airs on a matching weekday and week" matchingDateAirs
      it "does not air on the wrong weekday" wrongWeekdayDoesNotAir
      it "does not air in the wrong week of the month" wrongWeekDoesNotAir
      it "treats NULL weeks as every week" nullWeeksAirEveryWeek
      it "never airs with an empty week list" emptyWeeksNeverAir
      it "returns false, not NULL, for a one-time template" oneTimeIsFalseNotNull
      it "puts days 1 to 7 in week 1 and day 8 in week 2" weekBoundaries
      it "agrees with the Haskell model over four months" agreesWithHaskellModel

    describe "day_of_week_num" $
      it "agrees with dayOfWeekNumber for every enum value" dayNumbersAgree

    describe "query plans" $
      it "inlines both functions" bothFunctionsInline

--------------------------------------------------------------------------------
-- Fixed dates
--
-- August 2026 starts on a Saturday, so the 3rd is a Monday in week 1 and the
-- 10th is a Monday in week 2.

monday :: Int64
monday = dayOfWeekNumber Monday

firstMonday :: Day
firstMonday = fromGregorian 2026 8 3

secondMonday :: Day
secondMonday = fromGregorian 2026 8 10

tuesday :: Day
tuesday = fromGregorian 2026 8 4

--------------------------------------------------------------------------------
-- Statements

-- | Call @recurrence_airs_on@ once.
--
-- Both the day number and the week list are nullable, because a one-time
-- template holds NULL in both and callers rely on the result being false.
airsOn :: Maybe Int64 -> Maybe [Int64] -> Day -> Hasql.Statement () Bool
airsOn mDayNum mWeeks day =
  let query =
        interp
          False
          [sql| SELECT recurrence_airs_on(#{mDayNum}, #{mWeeks}, #{day}) |]
   in maybe False getOneColumn <$> query

-- | Every date in a range the recurrence covers.
--
-- One round trip per recurrence, rather than one per date.
airsOnDatesIn :: Int64 -> [Int64] -> Day -> Day -> Hasql.Statement () [Day]
airsOnDatesIn dayNum weeks from to =
  let query =
        interp
          False
          [sql|
        SELECT d::DATE
        FROM generate_series(#{from}::DATE, #{to}::DATE, INTERVAL '1 day') d
        WHERE recurrence_airs_on(#{dayNum}, #{weeks}, d::DATE)
        ORDER BY 1
      |]
   in fmap getOneColumn <$> query

-- | Call @day_of_week_num@ once.
--
-- The cast is required. hasql sends a DayOfWeek as an untyped parameter, so
-- Postgres sees text and finds no matching function. insertScheduleTemplate
-- casts for the same reason.
dayNumOf :: DayOfWeek -> Hasql.Statement () (Maybe Int64)
dayNumOf dow =
  let query =
        interp
          False
          [sql| SELECT day_of_week_num(#{dow}::day_of_week) |]
   in fmap getOneColumn <$> query

-- | The plan for a filter that calls both functions.
planFor :: Hasql.Statement () [Text]
planFor =
  let query =
        interp
          False
          [sql|
        EXPLAIN (COSTS OFF)
        SELECT id FROM schedule_templates st
        WHERE recurrence_airs_on(
                day_of_week_num(st.day_of_week),
                st.weeks_of_month,
                DATE '2026-08-03'
              )
      |]
   in fmap getOneColumn <$> query

--------------------------------------------------------------------------------
-- Helpers

runQuery :: TestDBConfig -> TRX.Transaction a -> IO a
runQuery cfg transaction = bracketConn cfg $ do
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read transaction
  case result of
    Left err -> error $ "DB error: " <> show err
    Right value -> pure value

-- | The Haskell statement of the same rule, for the cross-check below.
haskellAirsOn :: Int64 -> [Int64] -> Day -> Bool
haskellAirsOn dayNum weeks day =
  dayOfWeekNumber (Time.dayOfWeek day) == dayNum
    && weekOfMonth day `elem` weeks

--------------------------------------------------------------------------------
-- Truth table

matchingDateAirs :: TestDBConfig -> IO ()
matchingDateAirs cfg = do
  result <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) (Just [1, 3]) firstMonday
  result `shouldBe` True

wrongWeekdayDoesNotAir :: TestDBConfig -> IO ()
wrongWeekdayDoesNotAir cfg = do
  -- Same week of the month, one day later.
  result <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) (Just [1, 3]) tuesday
  result `shouldBe` False

wrongWeekDoesNotAir :: TestDBConfig -> IO ()
wrongWeekDoesNotAir cfg = do
  -- The right weekday is not enough. A first-and-third-Monday show skips the
  -- second Monday.
  result <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) (Just [1, 3]) secondMonday
  result `shouldBe` False

nullWeeksAirEveryWeek :: TestDBConfig -> IO ()
nullWeeksAirEveryWeek cfg = do
  -- The readers treat weeks_of_month IS NULL as no restriction.
  first <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) Nothing firstMonday
  second <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) Nothing secondMonday
  (first, second) `shouldBe` (True, True)

emptyWeeksNeverAir :: TestDBConfig -> IO ()
emptyWeeksNeverAir cfg = do
  -- An empty list is not the same as NULL. It describes a show that never airs,
  -- and it must stay visibly broken rather than quietly become every week.
  result <- runQuery cfg $ TRX.statement () $ airsOn (Just monday) (Just []) firstMonday
  result `shouldBe` False

oneTimeIsFalseNotNull :: TestDBConfig -> IO ()
oneTimeIsFalseNotNull cfg = do
  -- A one-time template holds NULL in both recurrence columns. Callers put this
  -- result inside CASE and on the left of OR, where NULL and false differ, so
  -- the function has to return false.
  result <- runQuery cfg $ TRX.statement () $ airsOn Nothing Nothing firstMonday
  result `shouldBe` False

weekBoundaries :: TestDBConfig -> IO ()
weekBoundaries cfg = do
  -- Weeks are fixed blocks of seven days from the 1st, not "the first Monday".
  -- The 29th to the 31st are week 5, which is why a week-5 show can skip a
  -- month.
  let dayAndWeek (dayOfMonth, week) = do
        let day = fromGregorian 2026 8 dayOfMonth
            dayNum = dayOfWeekNumber (Time.dayOfWeek day)
        airs <- runQuery cfg $ TRX.statement () $ airsOn (Just dayNum) (Just [week]) day
        (dayOfMonth, week, airs) `shouldBe` (dayOfMonth, week, True)
  forM_ [(7, 1), (8, 2), (28, 4), (29, 5), (31, 5)] dayAndWeek

  -- And the neighbouring week rejects each of those dates.
  let wrongWeek (dayOfMonth, week) = do
        let day = fromGregorian 2026 8 dayOfMonth
            dayNum = dayOfWeekNumber (Time.dayOfWeek day)
        airs <- runQuery cfg $ TRX.statement () $ airsOn (Just dayNum) (Just [week]) day
        (dayOfMonth, week, airs) `shouldBe` (dayOfMonth, week, False)
  forM_ [(7, 2), (8, 1), (28, 5), (29, 4), (31, 4)] wrongWeek

--------------------------------------------------------------------------------
-- Cross-checks

-- | The SQL function and the Haskell model must pick the same dates.
--
-- The window covers a 31-day month, February, and a 30-day month, so it
-- includes both a month with a week 5 and a month whose week 5 is a single day.
-- Any disagreement between the two implementations is exactly the drift this
-- rule kept producing when it lived in ten places.
agreesWithHaskellModel :: TestDBConfig -> IO ()
agreesWithHaskellModel cfg = do
  let from = fromGregorian 2026 1 1
      to = fromGregorian 2026 4 30
      allDays = [from .. to]
      weekSets = [[1], [3], [5], [1, 3], [2, 4], [1, 2, 3, 4, 5]]
      dayNums = map dayOfWeekNumber [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  forM_ dayNums $ \dayNum ->
    forM_ weekSets $ \weeks -> do
      actual <- runQuery cfg $ TRX.statement () $ airsOnDatesIn dayNum weeks from to
      let expected = filter (haskellAirsOn dayNum weeks) allDays
      (dayNum, weeks, actual) `shouldBe` (dayNum, weeks, expected)

-- | The SQL enum mapping and the Haskell one must agree.
--
-- Both exist to match @EXTRACT(DOW ...)@, where Sunday is 0. Data.Time numbers
-- Monday 1 through Sunday 7, so only Sunday moves, and a wrong mapping there
-- shifts every show by a day.
dayNumbersAgree :: TestDBConfig -> IO ()
dayNumbersAgree cfg =
  forM_ [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday] $ \dow -> do
    fromSql <- runQuery cfg $ TRX.statement () $ dayNumOf dow
    (dow, fromSql) `shouldBe` (dow, Just (dayOfWeekNumber dow))

--------------------------------------------------------------------------------
-- Query plans

-- | Postgres must expand both functions into the plan rather than call them.
--
-- Inlining is what keeps plans and index use the same as when the rule was
-- written out at each call site. It holds only while each body stays a single
-- IMMUTABLE expression. Casting the enum to text is enough to break it, because
-- enum_out is STABLE, and nothing else would report the loss.
bothFunctionsInline :: TestDBConfig -> IO ()
bothFunctionsInline cfg = do
  plan <- runQuery cfg $ TRX.statement () planFor
  let planText = Text.unlines plan
  planText `shouldSatisfy` \t -> not (Text.isInfixOf "recurrence_airs_on" t)
  planText `shouldSatisfy` \t -> not (Text.isInfixOf "day_of_week_num" t)
  -- The expanded rule is present instead.
  planText `shouldSatisfy` Text.isInfixOf "weeks_of_month"
