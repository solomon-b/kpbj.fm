module Effects.Database.Tables.SitePagesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePages qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (<==))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.SitePages" $ do
      describe "Queries" $ do
        runs 5 . it "getAllPages: returns pre-seeded pages" $ hedgehog . prop_getAllPages
        runs 5 . it "getPageBySlug: returns page for known slug" $ hedgehog . prop_getPageBySlug
        runs 5 . it "getPageBySlug: returns Nothing for unknown slug" $ hedgehog . prop_getPageBySlug_missing

      describe "Mutations" $ do
        runs 5 . it "updatePage: updates title and content" $ hedgehog . prop_updatePage

--------------------------------------------------------------------------------
-- Query tests

-- | getAllPages: returns pre-seeded pages ordered by slug.
prop_getAllPages :: TestDBConfig -> PropertyT IO ()
prop_getAllPages cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        pages <- TRX.statement () UUT.getAllPages
        TRX.condemn
        pure pages

      assert $ do
        pages <- assertRight result
        -- Pre-seeded pages should exist (about, privacy-policy, terms-of-service)
        length pages <== (> 0)
        pure ()

-- | getPageBySlug: returns a pre-seeded page.
prop_getPageBySlug :: TestDBConfig -> PropertyT IO ()
prop_getPageBySlug cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        -- Try to get a page - use "about" which should be pre-seeded
        mPage <- TRX.statement () (UUT.getPageBySlug "about")
        TRX.condemn
        pure mPage

      assert $ do
        mPage <- assertRight result
        page <- assertJust mPage
        UUT.spmSlug page === "about"
        pure ()

-- | getPageBySlug: returns Nothing for unknown slug.
prop_getPageBySlug_missing :: TestDBConfig -> PropertyT IO ()
prop_getPageBySlug_missing cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        mPage <- TRX.statement () (UUT.getPageBySlug "nonexistent-page-slug-12345")
        TRX.condemn
        pure mPage

      assert $ do
        mPage <- assertRight result
        assertNothing mPage
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | updatePage: updates title and content, returns updated model.
prop_updatePage :: TestDBConfig -> PropertyT IO ()
prop_updatePage cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Get the pre-seeded "about" page
        mPage <- TRX.statement () (UUT.getPageBySlug "about")

        case mPage of
          Nothing -> pure (Nothing, Nothing)
          Just page -> do
            let update = UUT.Update {UUT.spuTitle = "Updated About Title", UUT.spuContent = "Updated content body."}
            mUpdated <- TRX.statement () (UUT.updatePage (UUT.spmId page) update)
            TRX.condemn
            pure (Just page, mUpdated)

      assert $ do
        (mOriginal, mUpdated) <- assertRight result
        _ <- assertJust mOriginal
        updated <- assertJust mUpdated
        UUT.spmTitle updated === "Updated About Title"
        UUT.spmContent updated === "Updated content body."
        -- Slug should not change
        UUT.spmSlug updated === "about"
        pure ()
