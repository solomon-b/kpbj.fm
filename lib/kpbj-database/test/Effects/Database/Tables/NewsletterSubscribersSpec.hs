module Effects.Database.Tables.NewsletterSubscribersSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.NewsletterSubscribers qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
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
