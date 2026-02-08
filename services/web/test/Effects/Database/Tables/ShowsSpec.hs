module Effects.Database.Tables.ShowsSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton, (<==), (=\\=))
import Test.Gen.Tables.ShowTags (showTagInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Shows" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        describe "getShowBySlug" $ do
          runs 10 . it "returns show by slug" $ hedgehog . prop_getShowBySlug
        describe "getShowsFiltered" $ do
          runs 10 . it "returns all matching shows" $ hedgehog . prop_getShowsFiltered
          runs 10 . it "respects Limit" $ hedgehog . prop_getShowsFiltered_limit
          runs 10 . it "respects Offset" $ hedgehog . prop_getShowsFiltered_offset
        describe "getAllActiveShows" $ do
          runs 10 . it "filters by Active status" $ hedgehog . prop_getAllActiveShows
        describe "searchShows" $ do
          runs 10 . it "finds shows by title substring" $ hedgehog . prop_searchShows

      describe "Mutations" $ do
        runs 10 . it "softDeleteShow: sets deleted_at, getById returns Nothing" $
          hedgehog . prop_softDeleteShow
        runs 10 . it "softDeleteShow: second delete is no-op" $
          hedgehog . prop_softDeleteShow_idempotent

      describe "Constraints" $ do
        runs 10 . it "rejects duplicate slug on insert" $ hedgehog . prop_insertDuplicateSlug

      describe "Tag Operations" $ do
        runs 10 . it "addTagToShow and getTagsForShow" $ hedgehog . prop_addAndGetTagsForShow
        runs 10 . it "removeAllTagsFromShow: clears all tag assignments" $ hedgehog . prop_removeAllTagsFromShow

      describe "Admin Queries" $ do
        runs 10 . it "getShowsForUser: returns shows where user is host" $ hedgehog . prop_getShowsForUser
        runs 10 . it "getAllShowsWithHostInfo: includes host count and names" $ hedgehog . prop_getAllShowsWithHostInfo
        runs 10 . it "getShowsByStatusWithHostInfo: filters by status" $ hedgehog . prop_getShowsByStatusWithHostInfo
        runs 10 . it "searchShowsWithHostInfo: finds shows by title" $ hedgehog . prop_searchShowsWithHostInfo

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.siTitle insert === UUT.title model
  UUT.siSlug insert === UUT.slug model
  UUT.siDescription insert === UUT.description model
  UUT.siLogoUrl insert === UUT.logoUrl model
  UUT.siStatus insert === UUT.status model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertedId <- unwrapInsert (UUT.insertShow showInsert)
        selected <- TRX.statement () (UUT.getShowById insertedId)
        TRX.condemn
        pure (insertedId, selected)

      assert $ do
        (insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch showInsert selected
        UUT.id selected === insertedId

-- | Update-Select: update then select returns updated values.
prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    originalInsert <- forAllT showInsertGen
    updateInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow originalInsert)

        updateResult <- TRX.statement () (UUT.updateShow showId updateInsert)

        selected <- TRX.statement () (UUT.getShowById showId)
        TRX.condemn
        pure (showId, updateInsert, updateResult, selected)

      assert $ do
        (showId, updated, updateResult, mSelected) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === showId

        selected <- assertJust mSelected
        assertInsertFieldsMatch updated selected
        UUT.id selected === showId
        pure ()

-- | Update-Update: second update fully overwrites first.
prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    originalInsert <- forAllT showInsertGen
    updateAInsert <- forAllT showInsertGen
    updateBInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow originalInsert)

        _ <- TRX.statement () (UUT.updateShow showId updateAInsert)
        _ <- TRX.statement () (UUT.updateShow showId updateBInsert)

        selected <- TRX.statement () (UUT.getShowById showId)
        TRX.condemn
        pure (showId, updateBInsert, selected)

      assert $ do
        (showId, updateB, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updateB selected
        UUT.id selected === showId
        pure ()

--------------------------------------------------------------------------------
-- Query tests

prop_getShowBySlug :: TestDBConfig -> PropertyT IO ()
prop_getShowBySlug cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertedId <- unwrapInsert (UUT.insertShow showInsert)
        byId <- TRX.statement () (UUT.getShowById insertedId)
        bySlug <- TRX.statement () (UUT.getShowBySlug $ UUT.siSlug showInsert)
        TRX.condemn
        pure (insertedId, byId, bySlug)

      assert $ do
        (insertedId, mById, mBySlug) <- assertRight result
        byId <- assertJust mById
        bySlug <- assertJust mBySlug
        UUT.id byId === UUT.id bySlug
        UUT.slug byId === UUT.slug bySlug
        UUT.id bySlug === insertedId

-- | getShowsFiltered: returns matching shows (requires active hosts).
prop_getShowsFiltered :: TestDBConfig -> PropertyT IO ()
prop_getShowsFiltered cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = template1 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)

        -- Add hosts so shows appear in filtered results
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id2 userId ShowHost.Host

        filtered <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, id2, filtered)

      assert $ do
        (id1, id2, filtered) <- assertRight result
        length filtered === 2
        let returnedIds = map UUT.id filtered
        returnedIds =\\= [id1, id2]
        pure ()

-- | getShowsFiltered: respects Limit.
prop_getShowsFiltered_limit :: TestDBConfig -> PropertyT IO ()
prop_getShowsFiltered_limit cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen
    template3 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = template1 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}
        let show3 = template3 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)
        id3 <- unwrapInsert (UUT.insertShow show3)

        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id2 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id3 userId ShowHost.Host

        limited <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 2) (Offset 0))
        TRX.condemn
        pure limited

      assert $ do
        limited <- assertRight result
        length limited === 2
        pure ()

-- | getShowsFiltered: respects Offset.
prop_getShowsFiltered_offset :: TestDBConfig -> PropertyT IO ()
prop_getShowsFiltered_offset cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen
    template3 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = template1 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}
        let show3 = template3 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)
        id3 <- unwrapInsert (UUT.insertShow show3)

        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id2 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id3 userId ShowHost.Host

        allShows <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 10) (Offset 0))
        offset1 <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 10) (Offset 1))
        offset2 <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 10) (Offset 2))
        offset3 <- TRX.statement () (UUT.getShowsFiltered Nothing Nothing NameAZ (Limit 10) (Offset 3))
        TRX.condemn
        pure (allShows, offset1, offset2, offset3)

      assert $ do
        (allShows, offset1, offset2, offset3) <- assertRight result
        length allShows === 3
        length offset1 === 2
        length offset2 === 1
        length offset3 === 0
        pure ()

-- | getAllActiveShows: filters by Active status.
prop_getAllActiveShows :: TestDBConfig -> PropertyT IO ()
prop_getAllActiveShows cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let activeShow = template1 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let inactiveShow = template2 {UUT.siStatus = UUT.Inactive, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        activeId <- unwrapInsert (UUT.insertShow activeShow)
        _inactiveId <- unwrapInsert (UUT.insertShow inactiveShow)

        activeShows <- TRX.statement () UUT.getAllActiveShows
        TRX.condemn
        pure (activeId, activeShows)

      assert $ do
        (activeId, activeShows) <- assertRight result
        length activeShows === 1
        let returnedIds = map UUT.id activeShows
        elem activeId returnedIds === True
        pure ()

-- | searchShows: finds shows by title substring (requires active hosts).
prop_searchShows :: TestDBConfig -> PropertyT IO ()
prop_searchShows cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = template1 {UUT.siTitle = "UniqueSearchTerm Alpha", UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siTitle = "Completely Different Beta", UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)

        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id2 userId ShowHost.Host

        found <- TRX.statement () (UUT.searchShows (Search "UniqueSearchTerm") (Limit 10) (Offset 0))
        notFound <- TRX.statement () (UUT.searchShows (Search "NoSuchShowExists") (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, found, notFound)

      assert $ do
        (id1, found, notFound) <- assertRight result
        foundShow <- assertSingleton found
        UUT.id foundShow === id1
        length notFound === 0
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | softDeleteShow: sets deleted_at, getById returns Nothing after.
prop_softDeleteShow :: TestDBConfig -> PropertyT IO ()
prop_softDeleteShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow showInsert)

        deleteResult <- TRX.statement () (UUT.softDeleteShow showId)
        afterDelete <- TRX.statement () (UUT.getShowById showId)
        TRX.condemn
        pure (showId, deleteResult, afterDelete)

      assert $ do
        (showId, deleteResult, afterDelete) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === showId
        assertNothing afterDelete
        pure ()

-- | softDeleteShow: second delete is no-op (returns Nothing).
prop_softDeleteShow_idempotent :: TestDBConfig -> PropertyT IO ()
prop_softDeleteShow_idempotent cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow showInsert)

        firstDelete <- TRX.statement () (UUT.softDeleteShow showId)
        secondDelete <- TRX.statement () (UUT.softDeleteShow showId)
        TRX.condemn
        pure (showId, firstDelete, secondDelete)

      assert $ do
        (showId, firstDelete, secondDelete) <- assertRight result
        deletedId <- assertJust firstDelete
        deletedId === showId
        assertNothing secondDelete
        pure ()

--------------------------------------------------------------------------------
-- Constraint tests

-- | Rejects duplicate slug on insert.
prop_insertDuplicateSlug :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicateSlug cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- unwrapInsert (UUT.insertShow template1)
        -- Insert with same slug
        _ <- unwrapInsert (UUT.insertShow template2 {UUT.siSlug = UUT.siSlug template1})
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()

--------------------------------------------------------------------------------
-- Tag tests

-- | addTagToShow and getTagsForShow.
prop_addAndGetTagsForShow :: TestDBConfig -> PropertyT IO ()
prop_addAndGetTagsForShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    tagInsert1 <- forAllT showTagInsertGen
    tagInsert2 <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow showInsert)

        tagId1 <- unwrapInsert (ShowTags.insertShowTag tagInsert1 {ShowTags.stiName = ShowTags.stiName tagInsert1 <> "1"})
        tagId2 <- unwrapInsert (ShowTags.insertShowTag tagInsert2 {ShowTags.stiName = ShowTags.stiName tagInsert2 <> "2"})

        TRX.statement () (UUT.addTagToShow showId tagId1)
        TRX.statement () (UUT.addTagToShow showId tagId2)

        tags <- TRX.statement () (UUT.getTagsForShow showId)

        -- Idempotent add
        TRX.statement () (UUT.addTagToShow showId tagId1)
        tagsAfterDupe <- TRX.statement () (UUT.getTagsForShow showId)

        TRX.condemn
        pure (tagInsert1, tagInsert2, tags, tagsAfterDupe)

      assert $ do
        (ti1, ti2, tags, tagsAfterDupe) <- assertRight result
        length tags === 2
        map ShowTags.stName tags =\\= [ShowTags.stiName ti1 <> "1", ShowTags.stiName ti2 <> "2"]
        length tagsAfterDupe === 2
        pure ()

-- | removeAllTagsFromShow: clears all tag assignments.
prop_removeAllTagsFromShow :: TestDBConfig -> PropertyT IO ()
prop_removeAllTagsFromShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    tagInsert1 <- forAllT showTagInsertGen
    tagInsert2 <- forAllT showTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (UUT.insertShow showInsert)

        tagId1 <- unwrapInsert (ShowTags.insertShowTag tagInsert1 {ShowTags.stiName = ShowTags.stiName tagInsert1 <> "1"})
        tagId2 <- unwrapInsert (ShowTags.insertShowTag tagInsert2 {ShowTags.stiName = ShowTags.stiName tagInsert2 <> "2"})

        TRX.statement () (UUT.addTagToShow showId tagId1)
        TRX.statement () (UUT.addTagToShow showId tagId2)

        tagsBefore <- TRX.statement () (UUT.getTagsForShow showId)

        -- Remove all tags
        TRX.statement () (UUT.removeAllTagsFromShow showId)

        tagsAfter <- TRX.statement () (UUT.getTagsForShow showId)

        TRX.condemn
        pure (tagsBefore, tagsAfter)

      assert $ do
        (tagsBefore, tagsAfter) <- assertRight result
        length tagsBefore === 2
        length tagsAfter === 0
        pure ()

--------------------------------------------------------------------------------
-- Admin Query tests

-- | getShowsForUser: returns shows where user is an active host.
prop_getShowsForUser :: TestDBConfig -> PropertyT IO ()
prop_getShowsForUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId1 <- insertTestUser userWithMetadata1
        userId2 <- insertTestUser userWithMetadata2

        let show1 = template1 {UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)

        -- User1 hosts show1, User2 hosts show2
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId1 ShowHost.Host
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id2 userId2 ShowHost.Host

        showsForUser1 <- TRX.statement () (UUT.getShowsForUser userId1)
        showsForUser2 <- TRX.statement () (UUT.getShowsForUser userId2)

        TRX.condemn
        pure (id1, id2, showsForUser1, showsForUser2)

      assert $ do
        (id1, id2, showsForUser1, showsForUser2) <- assertRight result
        -- User1 only sees show1
        s1 <- assertSingleton showsForUser1
        UUT.id s1 === id1
        -- User2 only sees show2
        s2 <- assertSingleton showsForUser2
        UUT.id s2 === id2
        pure ()

-- | getAllShowsWithHostInfo: includes host count and names.
prop_getAllShowsWithHostInfo :: TestDBConfig -> PropertyT IO ()
prop_getAllShowsWithHostInfo cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = template1 {UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        id2 <- unwrapInsert (UUT.insertShow show2)

        -- Add host to show1 only
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert id1 userId ShowHost.Host

        allShows <- TRX.statement () (UUT.getAllShowsWithHostInfo (Limit 10) (Offset 0))

        TRX.condemn
        pure (id1, id2, allShows)

      assert $ do
        (id1, id2, allShows) <- assertRight result
        length allShows === 2
        let returnedIds = map UUT.swhiId allShows
        returnedIds =\\= [id1, id2]
        -- Show1 should have 1 host, show2 should have 0
        let show1Info = filter (\s -> UUT.swhiId s == id1) allShows
        let show2Info = filter (\s -> UUT.swhiId s == id2) allShows
        s1 <- assertSingleton show1Info
        s2 <- assertSingleton show2Info
        UUT.swhiHostCount s1 === 1
        UUT.swhiHostCount s2 === 0
        pure ()

-- | getShowsByStatusWithHostInfo: filters by status.
prop_getShowsByStatusWithHostInfo :: TestDBConfig -> PropertyT IO ()
prop_getShowsByStatusWithHostInfo cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let activeShow = template1 {UUT.siStatus = UUT.Active, UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let inactiveShow = template2 {UUT.siStatus = UUT.Inactive, UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        activeId <- unwrapInsert (UUT.insertShow activeShow)
        _inactiveId <- unwrapInsert (UUT.insertShow inactiveShow)

        activeShows <- TRX.statement () (UUT.getShowsByStatusWithHostInfo UUT.Active (Limit 10) (Offset 0))
        inactiveShows <- TRX.statement () (UUT.getShowsByStatusWithHostInfo UUT.Inactive (Limit 10) (Offset 0))

        TRX.condemn
        pure (activeId, activeShows, inactiveShows)

      assert $ do
        (activeId, activeShows, inactiveShows) <- assertRight result
        -- Only active show returned for Active filter
        activeShow <- assertSingleton activeShows
        UUT.swhiId activeShow === activeId
        -- Only inactive show returned for Inactive filter
        length inactiveShows === 1
        pure ()

-- | searchShowsWithHostInfo: finds shows by title substring.
prop_searchShowsWithHostInfo :: TestDBConfig -> PropertyT IO ()
prop_searchShowsWithHostInfo cfg = do
  arrange (bracketConn cfg) $ do
    template1 <- forAllT showInsertGen
    template2 <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = template1 {UUT.siTitle = "AdminSearchableUnique Alpha", UUT.siSlug = UUT.siSlug template1 <> Slug "1"}
        let show2 = template2 {UUT.siTitle = "Completely Different Beta", UUT.siSlug = UUT.siSlug template2 <> Slug "2"}

        id1 <- unwrapInsert (UUT.insertShow show1)
        _id2 <- unwrapInsert (UUT.insertShow show2)

        found <- TRX.statement () (UUT.searchShowsWithHostInfo (Search "AdminSearchableUnique") (Limit 10) (Offset 0))
        notFound <- TRX.statement () (UUT.searchShowsWithHostInfo (Search "ZZNoSuchShowExists") (Limit 10) (Offset 0))

        TRX.condemn
        pure (id1, found, notFound)

      assert $ do
        (id1, found, notFound) <- assertRight result
        foundShow <- assertSingleton found
        UUT.swhiId foundShow === id1
        length notFound === 0
        pure ()
