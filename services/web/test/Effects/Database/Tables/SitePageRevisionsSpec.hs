module Effects.Database.Tables.SitePageRevisionsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePageRevisions qualified as UUT
import Effects.Database.Tables.SitePages qualified as SitePages
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, annotate, failure, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight, (->-))
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.SitePageRevisions" $ do
      describe "Lens Laws" $ do
        runs 5 . it "insert-select: inserted revision retrievable by ID" $
          hedgehog . prop_insertRevision

      describe "Queries" $ do
        runs 5 . it "getRevisionsForPage: returns revisions ordered DESC by created_at" $
          hedgehog . prop_getRevisionsForPage
        runs 5 . it "getRevisionById: returns specific revision" $
          hedgehog . prop_getRevisionById

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert a revision and retrieve it by ID.
prop_insertRevision :: TestDBConfig -> PropertyT IO ()
prop_insertRevision cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Get a pre-seeded page to attach revision to
        mPage <- TRX.statement () (SitePages.getPageBySlug "about")

        case mPage of
          Nothing -> pure (Nothing, Nothing)
          Just page -> do
            let revInsert =
                  UUT.Insert
                    { UUT.spriPageId = SitePages.spmId page,
                      UUT.spriContent = "Revision content for test.",
                      UUT.spriEditSummary = Just "Test edit summary",
                      UUT.spriCreatedBy = userId
                    }
            mRevId <- TRX.statement () (UUT.insertRevision revInsert)

            case mRevId of
              Nothing -> do
                TRX.condemn
                pure (Nothing, Nothing)
              Just revId -> do
                selected <- TRX.statement () (UUT.getRevisionById revId)
                TRX.condemn
                pure (Just revId, selected)

      assert $ do
        (mRevId, mSelected) <- assertRight result
        revId <- assertJust mRevId
        selected <- assertJust mSelected
        UUT.sprId selected === revId
        UUT.sprContent selected === "Revision content for test."
        UUT.sprEditSummary selected === Just "Test edit summary"
        revId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getRevisionsForPage: returns revisions ordered by created_at DESC.
prop_getRevisionsForPage :: TestDBConfig -> PropertyT IO ()
prop_getRevisionsForPage cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        mPage <- TRX.statement () (SitePages.getPageBySlug "about")

        case mPage of
          Nothing -> pure (Nothing, [])
          Just page -> do
            let pageId = SitePages.spmId page

            -- Insert two revisions
            let rev1 =
                  UUT.Insert
                    { UUT.spriPageId = pageId,
                      UUT.spriContent = "First revision",
                      UUT.spriEditSummary = Just "First",
                      UUT.spriCreatedBy = userId
                    }
            let rev2 =
                  UUT.Insert
                    { UUT.spriPageId = pageId,
                      UUT.spriContent = "Second revision",
                      UUT.spriEditSummary = Just "Second",
                      UUT.spriCreatedBy = userId
                    }

            mRevId1 <- TRX.statement () (UUT.insertRevision rev1)
            mRevId2 <- TRX.statement () (UUT.insertRevision rev2)

            revisions <- TRX.statement () (UUT.getRevisionsForPage pageId)
            TRX.condemn
            pure (Just (mRevId1, mRevId2), revisions)

      assert $ do
        (mIds, revisions) <- assertRight result
        (mRevId1, mRevId2) <- assertJust mIds
        _ <- assertJust mRevId1
        _ <- assertJust mRevId2

        -- Most recent should be first (DESC order); there may be others from migrations
        case revisions of
          (firstRev : _ : _) ->
            UUT.sprContent (UUT.rweRevision firstRev) === "Second revision"
          _ -> do
            annotate $ "Expected at least 2 revisions but found " <> show (length revisions)
            failure

-- | getRevisionById: returns specific revision by ID.
prop_getRevisionById :: TestDBConfig -> PropertyT IO ()
prop_getRevisionById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        mPage <- TRX.statement () (SitePages.getPageBySlug "about")

        case mPage of
          Nothing -> pure (Nothing, Nothing)
          Just page -> do
            let revInsert =
                  UUT.Insert
                    { UUT.spriPageId = SitePages.spmId page,
                      UUT.spriContent = "Specific revision content",
                      UUT.spriEditSummary = Nothing,
                      UUT.spriCreatedBy = userId
                    }
            mRevId <- TRX.statement () (UUT.insertRevision revInsert)

            case mRevId of
              Nothing -> do
                TRX.condemn
                pure (Nothing, Nothing)
              Just revId -> do
                selected <- TRX.statement () (UUT.getRevisionById revId)
                TRX.condemn
                pure (Just revId, selected)

      assert $ do
        (mRevId, mSelected) <- assertRight result
        revId <- assertJust mRevId
        selected <- assertJust mSelected
        UUT.sprId selected === revId
        UUT.sprContent selected === "Specific revision content"
        UUT.sprEditSummary selected === Nothing
        pure ()
