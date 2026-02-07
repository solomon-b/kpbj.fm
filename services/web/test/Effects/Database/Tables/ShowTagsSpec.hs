module Effects.Database.Tables.ShowTagsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowTags qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton, (->-), (<==))
import Test.Gen.Tables.ShowTags (showTagInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowTags" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        describe "getShowTagByName" $ do
          runs 10 . it "returns Nothing for missing tag" $ hedgehog . prop_getTagByNameMissing
        describe "insertShowTag" $ do
          runs 10 . it "rejects duplicate name" $ hedgehog . prop_insertDuplicateName

      describe "Aggregation" $ do
        runs 10 . it "getShowTagsWithCounts: returns tags with active show counts" $
          hedgehog . prop_getShowTagsWithCounts
        runs 10 . it "getShowTagsWithCounts: excludes inactive shows" $
          hedgehog . prop_getShowTagsWithCounts_activeOnly

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.stiName insert === UUT.stName model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select (PutGet): insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        tagId <- TRX.statement () (UUT.insertShowTag tagInsert)
        selected <- TRX.statement () (UUT.getShowTagByName (UUT.stiName tagInsert))
        TRX.condemn
        pure (tagId, tagInsert, selected)

      assert $ do
        (tagId, insert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch insert selected
        UUT.stId selected === tagId
        tagId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getShowTagByName returns Nothing for a name that was never inserted.
prop_getTagByNameMissing :: TestDBConfig -> PropertyT IO ()
prop_getTagByNameMissing cfg = do
  arrange (bracketConn cfg) $ do
    tagInsert <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        selected <- TRX.statement () (UUT.getShowTagByName (UUT.stiName tagInsert))
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
    tagInsert <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- TRX.statement () (UUT.insertShowTag tagInsert)
        _ <- TRX.statement () (UUT.insertShowTag tagInsert)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()

--------------------------------------------------------------------------------
-- Aggregation tests

-- | getShowTagsWithCounts: returns tags with correct counts for active shows.
prop_getShowTagsWithCounts :: TestDBConfig -> PropertyT IO ()
prop_getShowTagsWithCounts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    tagInsert <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Create an active show
        let activeShow = showInsert {Shows.siStatus = Shows.Active}
        showId <- TRX.statement () (Shows.insertShow activeShow)

        -- Add host so show is valid
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert showId userId ShowHost.Host

        -- Create and assign tag
        tagId <- TRX.statement () (UUT.insertShowTag tagInsert)
        TRX.statement () (Shows.addTagToShow showId tagId)

        -- Query with counts
        tagsWithCounts <- TRX.statement () UUT.getShowTagsWithCounts
        TRX.condemn
        pure (tagId, tagsWithCounts)

      assert $ do
        (tagId, tagsWithCounts) <- assertRight result
        -- Should have at least our tag
        tag <- assertSingleton tagsWithCounts
        UUT.stwcId tag === tagId
        UUT.stwcCount tag === 1
        pure ()

-- | getShowTagsWithCounts: excludes inactive shows from counts.
prop_getShowTagsWithCounts_activeOnly :: TestDBConfig -> PropertyT IO ()
prop_getShowTagsWithCounts_activeOnly cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    activeTemplate <- forAllT showInsertGen
    inactiveTemplate <- forAllT showInsertGen
    tagInsert <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Create active and inactive shows with distinct slugs
        let activeShow = activeTemplate {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug activeTemplate <> "1"}
        let inactiveShow = inactiveTemplate {Shows.siStatus = Shows.Inactive, Shows.siSlug = Shows.siSlug inactiveTemplate <> "2"}

        activeId <- TRX.statement () (Shows.insertShow activeShow)
        inactiveId <- TRX.statement () (Shows.insertShow inactiveShow)

        -- Add hosts
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert activeId userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert inactiveId userId ShowHost.Host

        -- Create tag and assign to both shows
        tagId <- TRX.statement () (UUT.insertShowTag tagInsert)
        TRX.statement () (Shows.addTagToShow activeId tagId)
        TRX.statement () (Shows.addTagToShow inactiveId tagId)

        -- Query - should only count active show
        tagsWithCounts <- TRX.statement () UUT.getShowTagsWithCounts
        TRX.condemn
        pure (tagId, tagsWithCounts)

      assert $ do
        (tagId, tagsWithCounts) <- assertRight result
        tag <- assertSingleton tagsWithCounts
        UUT.stwcId tag === tagId
        -- Only the active show should be counted
        UUT.stwcCount tag === 1
        pure ()
