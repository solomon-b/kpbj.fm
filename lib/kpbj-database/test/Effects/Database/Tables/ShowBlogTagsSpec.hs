module Effects.Database.Tables.ShowBlogTagsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogTags qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (->-), (<==))
import Test.Gen.Tables.ShowBlogTags (showBlogTagInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowBlogTags" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        describe "getShowBlogTagByName" $ do
          runs 10 . it "returns Nothing for missing tag" $ hedgehog . prop_getTagByNameMissing
        describe "insertShowBlogTag" $ do
          runs 10 . it "rejects duplicate name" $ hedgehog . prop_insertDuplicateName

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.sbtiName insert === UUT.sbtmName model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select (PutGet): insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        tagId <- unwrapInsert (UUT.insertShowBlogTag tagInsert)
        selected <- TRX.statement () (UUT.getShowBlogTagByName (UUT.sbtiName tagInsert))
        TRX.condemn
        pure (tagId, tagInsert, selected)

      assert $ do
        (tagId, insert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch insert selected
        UUT.sbtmId selected === tagId
        tagId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getShowBlogTagByName returns Nothing for a name that was never inserted.
prop_getTagByNameMissing :: TestDBConfig -> PropertyT IO ()
prop_getTagByNameMissing cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        selected <- TRX.statement () (UUT.getShowBlogTagByName (UUT.sbtiName tagInsert))
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
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- unwrapInsert (UUT.insertShowBlogTag tagInsert)
        -- Insert with same name (should fail due to unique constraint)
        _ <- unwrapInsert (UUT.insertShowBlogTag tagInsert)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()
