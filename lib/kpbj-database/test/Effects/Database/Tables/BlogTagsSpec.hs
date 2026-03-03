module Effects.Database.Tables.BlogTagsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogTags qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (->-), (<==))
import Test.Gen.Tables.BlogTags (blogTagInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BlogTags" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        describe "getTagByName" $ do
          runs 10 . it "returns Nothing for missing tag" $ hedgehog . prop_getTagByNameMissing
        describe "insertTag" $ do
          runs 10 . it "rejects duplicate name" $ hedgehog . prop_insertDuplicateName

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.btiName insert === UUT.btmName model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select (PutGet): insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        tagId <- unwrapInsert (UUT.insertTag tagInsert)
        selected <- TRX.statement () (UUT.getTagByName (UUT.btiName tagInsert))
        TRX.condemn
        pure (tagId, tagInsert, selected)

      assert $ do
        (tagId, insert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch insert selected
        UUT.btmId selected === tagId
        tagId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getTagByName returns Nothing for a name that was never inserted.
prop_getTagByNameMissing :: TestDBConfig -> PropertyT IO ()
prop_getTagByNameMissing cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        selected <- TRX.statement () (UUT.getTagByName (UUT.btiName tagInsert))
        TRX.condemn
        pure selected

      assert $ do
        selected <- assertRight result
        assertNothing selected
        pure ()

-- | Inserting two tags with the same name violates unique constraint.
prop_insertDuplicateName :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicateName cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- unwrapInsert (UUT.insertTag tagInsert)
        -- Insert with same name (should fail due to unique constraint)
        _ <- unwrapInsert (UUT.insertTag tagInsert)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()
