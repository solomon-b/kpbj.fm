{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.NewsletterSubscribersSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Data.Time (UTCTime (..), fromGregorian)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.NewsletterSubscribers qualified as UUT
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.NewsletterSubscribers (newsletterSubscriberInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.NewsletterSubscribers" $ do
      describe "insert" $ do
        runs 10 . it "inserts a fresh email and returns its id" $
          hedgehog . prop_insertFresh
        runs 10 . it "silently no-ops on duplicate email" $
          hedgehog . prop_insertDuplicate
      describe "getDailySignups, countCreatedBefore, and getFirstCreatedAt" $
        runs 3 . it "count signups by day in the window and before it, and find the first signup" $
          hedgehog . prop_signups

--------------------------------------------------------------------------------

-- | Inserting a fresh email returns Just an id.
prop_insertFresh :: TestDBConfig -> PropertyT IO ()
prop_insertFresh cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT newsletterSubscriberInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertedId <- unwrapInsert (UUT.insert template)
        TRX.condemn
        pure insertedId

      assert $ do
        _insertedId <- assertRight result
        pure ()

-- | Inserting the same email twice returns Nothing on the second insert
-- and leaves exactly one row.
prop_insertDuplicate :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicate cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT newsletterSubscriberInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        firstResult <- TRX.statement () (UUT.insert template)
        secondResult <- TRX.statement () (UUT.insert template)
        countAfter <- TRX.statement () (UUT.countByEmail (UUT.nsiEmail template))
        TRX.condemn
        pure (firstResult, secondResult, countAfter)

      assert $ do
        (firstResult, secondResult, countAfter) <- assertRight result
        _ <- assertJust firstResult
        assertNothing secondResult
        countAfter === 1
        pure ()

-- | One subscriber signed up before June 2026 and two signed up on
-- 2026-06-10 in station time. The second signup is at 03:00 UTC on
-- 2026-06-11, which is 20:00 PDT on 2026-06-10.
prop_signups :: TestDBConfig -> PropertyT IO ()
prop_signups cfg = do
  arrange (bracketConn cfg) $ do
    templates <- forAllT $ Gen.list (Range.singleton 3) newsletterSubscriberInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        ids <- traverse (unwrapInsert . UUT.insert) templates
        let dates = [utc 2026 5 1, utcAt 2026 6 10 12, utcAt 2026 6 11 3]
        forM_ (zip ids dates) (uncurry setCreatedAt)
        before <- TRX.statement () (UUT.countCreatedBefore (utc 2026 6 1))
        daily <- TRX.statement () (UUT.getDailySignups (utc 2026 6 1) (utc 2026 7 1))
        first <- TRX.statement () UUT.getFirstCreatedAt
        TRX.condemn
        pure (before, daily, first)

      assert $ do
        (before, daily, first) <- assertRight result
        before === 1
        daily === [(2, "2026-06-10")]
        first === Just (utc 2026 5 1)

setCreatedAt :: UUT.Id -> UTCTime -> TRX.Transaction ()
setCreatedAt subscriberId createdAt =
  TRX.statement () $
    interp
      False
      [sql| UPDATE newsletter_subscribers SET created_at = #{createdAt} WHERE id = #{subscriberId} |]

utc :: Integer -> Int -> Int -> UTCTime
utc y m d = utcAt y m d 0

-- | A time on the hour in UTC.
utcAt :: Integer -> Int -> Int -> Int -> UTCTime
utcAt y m d h = UTCTime (fromGregorian y m d) (fromIntegral (h * 3600))
