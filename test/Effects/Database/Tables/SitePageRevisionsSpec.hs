module Effects.Database.Tables.SitePageRevisionsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePageRevisions qualified as UUT
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.SitePageRevisions (sitePageRevisionInsertGen)
import Test.Gen.Tables.SitePages (sitePageInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.SitePageRevisions" $ do
      runs 10 . it "lens law: insert-select revision" $ hedgehog . prop_insertSelectRevision
      runs 10 . it "lens law: getRevisionById returns inserted revision" $ hedgehog . prop_getRevisionById
      runs 10 . it "query: getRevisionsForPage returns all revisions for a page" $ hedgehog . prop_getRevisionsForPage
      runs 10 . it "query: getLatestRevision returns most recent revision" $ hedgehog . prop_getLatestRevision
      runs 10 . it "query: countRevisionsForPage returns correct count" $ hedgehog . prop_countRevisionsForPage

-- Lens Law: insert then select returns what we inserted
prop_insertSelectRevision :: TestDBConfig -> PropertyT IO ()
prop_insertSelectRevision cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    pageTemplate <- forAllT $ sitePageInsertGen
    revisionTemplate <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Create page
        let pageInsert = pageTemplate
        mPageId <- TRX.statement () (SitePages.insertPage pageInsert)

        case mPageId of
          Nothing -> pure Nothing
          Just pageId -> do
            -- Create revision
            let revisionInsert = revisionTemplate {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
            mRevisionId <- TRX.statement () (UUT.insertRevision revisionInsert)
            case mRevisionId of
              Nothing -> pure Nothing
              Just revisionId -> do
                selected <- TRX.statement () (UUT.getRevisionById revisionId)
                pure $ Just (revisionId, revisionInsert, selected)

      assert $ do
        mResult <- assertRight result
        (revisionId, revisionInsert, mSelected) <- assertJust mResult
        selected <- assertJust mSelected
        UUT.spriContent revisionInsert === UUT.sprContent selected
        UUT.spriEditSummary revisionInsert === UUT.sprEditSummary selected
        UUT.spriPageId revisionInsert === UUT.sprPageId selected
        revisionId === UUT.sprId selected
        pure ()

-- Lens Law: getRevisionById after insert returns the revision
prop_getRevisionById :: TestDBConfig -> PropertyT IO ()
prop_getRevisionById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    pageTemplate <- forAllT $ sitePageInsertGen
    revisionTemplate <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Create page
        let pageInsert = pageTemplate
        mPageId <- TRX.statement () (SitePages.insertPage pageInsert)

        case mPageId of
          Nothing -> pure Nothing
          Just pageId -> do
            -- Create revision
            let revisionInsert = revisionTemplate {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
            mRevisionId <- TRX.statement () (UUT.insertRevision revisionInsert)
            case mRevisionId of
              Nothing -> pure Nothing
              Just revisionId -> do
                byId <- TRX.statement () (UUT.getRevisionById revisionId)
                pure $ Just (revisionId, byId)

      assert $ do
        mResult <- assertRight result
        (revisionId, mById) <- assertJust mResult
        byId <- assertJust mById
        UUT.sprId byId === revisionId
        pure ()

-- Test: getRevisionsForPage returns all revisions for a specific page
prop_getRevisionsForPage :: TestDBConfig -> PropertyT IO ()
prop_getRevisionsForPage cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    pageTemplate <- forAllT $ sitePageInsertGen
    revisionTemplate1 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)
    revisionTemplate2 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Create page
        let pageInsert = pageTemplate
        mPageId <- TRX.statement () (SitePages.insertPage pageInsert)

        case mPageId of
          Nothing -> pure Nothing
          Just pageId -> do
            -- Create two revisions
            let revisionInsert1 = revisionTemplate1 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
                revisionInsert2 = revisionTemplate2 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
            mRevisionId1 <- TRX.statement () (UUT.insertRevision revisionInsert1)
            mRevisionId2 <- TRX.statement () (UUT.insertRevision revisionInsert2)

            case (mRevisionId1, mRevisionId2) of
              (Just revisionId1, Just revisionId2) -> do
                revisions <- TRX.statement () (UUT.getRevisionsForPage pageId)
                pure $ Just (pageId, revisionId1, revisionId2, revisions)
              _ -> pure Nothing

      assert $ do
        mResult <- assertRight result
        (pageId, revisionId1, revisionId2, revisions) <- assertJust mResult
        -- Both revisions should be returned
        let revisionIds = map (UUT.sprId . UUT.rweRevision) revisions
            allForCorrectPage = all (\r -> UUT.sprPageId (UUT.rweRevision r) == pageId) revisions
        (revisionId1 `elem` revisionIds) === True
        (revisionId2 `elem` revisionIds) === True
        allForCorrectPage === True
        pure ()

-- Test: getLatestRevision returns the most recent revision
prop_getLatestRevision :: TestDBConfig -> PropertyT IO ()
prop_getLatestRevision cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    pageTemplate <- forAllT $ sitePageInsertGen
    revisionTemplate1 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)
    revisionTemplate2 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Create page
        let pageInsert = pageTemplate
        mPageId <- TRX.statement () (SitePages.insertPage pageInsert)

        case mPageId of
          Nothing -> pure Nothing
          Just pageId -> do
            -- Create two revisions (second one is newer)
            let revisionInsert1 = revisionTemplate1 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
                revisionInsert2 = revisionTemplate2 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
            _ <- TRX.statement () (UUT.insertRevision revisionInsert1)
            mRevisionId2 <- TRX.statement () (UUT.insertRevision revisionInsert2)

            case mRevisionId2 of
              Just revisionId2 -> do
                mLatest <- TRX.statement () (UUT.getLatestRevision pageId)
                allRevisions <- TRX.statement () (UUT.getRevisionsForPage pageId)
                pure $ Just (pageId, revisionId2, mLatest, allRevisions)
              _ -> pure Nothing

      assert $ do
        mResult <- assertRight result
        (pageId, revisionId2, mLatest, allRevisions) <- assertJust mResult
        latest <- assertJust mLatest

        -- Latest should be for the correct page
        UUT.sprPageId latest === pageId

        -- Latest should be the second revision (inserted last, so highest ID)
        UUT.sprId latest === revisionId2

        -- allRevisions is already sorted by (created_at DESC, id DESC) from query
        -- so the first element should match latest
        case allRevisions of
          (mostRecent : _) -> UUT.sprId (UUT.rweRevision mostRecent) === UUT.sprId latest
          [] -> pure () -- No revisions case (shouldn't happen in this test)
        pure ()

-- Test: countRevisionsForPage returns correct count
prop_countRevisionsForPage :: TestDBConfig -> PropertyT IO ()
prop_countRevisionsForPage cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    pageTemplate <- forAllT $ sitePageInsertGen
    revisionTemplate1 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)
    revisionTemplate2 <- forAllT $ sitePageRevisionInsertGen (SitePages.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Create page
        let pageInsert = pageTemplate
        mPageId <- TRX.statement () (SitePages.insertPage pageInsert)

        case mPageId of
          Nothing -> pure Nothing
          Just pageId -> do
            -- Count before any revisions
            countBefore <- TRX.statement () (UUT.countRevisionsForPage pageId)

            -- Create two revisions
            let revisionInsert1 = revisionTemplate1 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
                revisionInsert2 = revisionTemplate2 {UUT.spriPageId = pageId, UUT.spriCreatedBy = userId}
            _ <- TRX.statement () (UUT.insertRevision revisionInsert1)
            _ <- TRX.statement () (UUT.insertRevision revisionInsert2)

            -- Count after revisions
            countAfter <- TRX.statement () (UUT.countRevisionsForPage pageId)
            pure $ Just (countBefore, countAfter)

      assert $ do
        mResult <- assertRight result
        (countBefore, countAfter) <- assertJust mResult
        countBefore === 0
        countAfter === 2
        pure ()
