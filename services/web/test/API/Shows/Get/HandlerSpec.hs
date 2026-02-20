module API.Shows.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Shows.Get.Handler (action)
import API.Shows.Get.Templates.Page (ShowsViewData (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Domain.Types.Filter (Filter (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, (=>=))
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Shows.Get.Handler" $ do
      describe "action" $ do
        runs 1 . it "returns empty view data on empty DB" $
          hedgehog . prop_emptyDB

        runs 5 . it "returns shows that have hosts" $
          hedgehog . prop_showsWithHosts

        runs 1 . it "pagination: hasMore flag when more than page size" $
          hedgehog . prop_pagination

        runs 5 . it "returns Right on success" $
          hedgehog . prop_returnsRight

        runs 1 . it "status filter excludes non-matching shows" $
          hedgehog . prop_statusFilter

--------------------------------------------------------------------------------

-- | Empty DB returns empty view data with no shows, no tags, no more pages.
prop_emptyDB :: TestDBConfig -> PropertyT IO ()
prop_emptyDB cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT $ action Nothing Nothing Nothing Nothing Nothing
      assert $ do
        vd <- assertRight result
        svShows vd === []
        svTags vd === []
        svHasMore vd === False

-- | Inserting a show with an active host increases the result count.
prop_showsWithHosts :: TestDBConfig -> PropertyT IO ()
prop_showsWithHosts cfg = do
  arrange (bracketAppM cfg) $ do
    showInsert <- forAllT showInsertGen
    userInsert <- forAllT userWithMetadataInsertGen

    act $ do
      -- Count shows before insert
      beforeResult <- runExceptT $ action Nothing Nothing Nothing Nothing Nothing
      let beforeCount = either (const 0) (length . svShows) beforeResult

      -- Setup: insert user, show, and make user a host of the show
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        showId <- unwrapInsert (Shows.insertShow showInsert)
        TRX.statement () (ShowHost.addHostToShow showId userId)

      -- Count shows after insert
      afterResult <- runExceptT $ action Nothing Nothing Nothing Nothing Nothing

      assert $ do
        vd <- assertRight afterResult
        length (svShows vd) === beforeCount + 1

-- | When more than 12 shows exist, hasMore is True on page 1, and page 2
-- returns the remaining shows.
prop_pagination :: TestDBConfig -> PropertyT IO ()
prop_pagination cfg = do
  arrange (bracketAppM cfg) $ do
    showInserts <- forAllT $ Gen.list (Range.singleton 13) showInsertGen
    userInsert <- forAllT userWithMetadataInsertGen

    act $ do
      -- Setup: insert user and 13 shows, each with the user as host
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        mapM_
          ( \si -> do
              showId <- unwrapInsert (Shows.insertShow si)
              TRX.statement () (ShowHost.addHostToShow showId userId)
          )
          showInserts

      -- Page 1: should have 12 shows and hasMore = True
      result1 <- runExceptT $ action (Just 1) Nothing Nothing Nothing Nothing
      -- Page 2: should have remaining shows
      result2 <- runExceptT $ action (Just 2) Nothing Nothing Nothing Nothing

      assert $ do
        vd1 <- assertRight result1
        svHasMore vd1 === True
        length (svShows vd1) === 12

        vd2 <- assertRight result2
        -- Page 2 has the overflow (at least 1 show)
        length (svShows vd2) =>= 1

-- | Action always succeeds (returns Right) on a valid database.
prop_returnsRight :: TestDBConfig -> PropertyT IO ()
prop_returnsRight cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT $ action Nothing Nothing Nothing Nothing Nothing
      assert $ do
        H.assert (isRight result)

-- | Filtering by status only returns shows with matching status.
prop_statusFilter :: TestDBConfig -> PropertyT IO ()
prop_statusFilter cfg = do
  arrange (bracketAppM cfg) $ do
    activeInsert <- forAllT showInsertGen
    inactiveInsert <- forAllT showInsertGen
    userInsert <- forAllT userWithMetadataInsertGen

    act $ do
      -- Setup: insert one active and one inactive show, both with a host
      _ <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert

        activeId <- unwrapInsert (Shows.insertShow activeInsert {Shows.siStatus = Shows.Active})
        TRX.statement () (ShowHost.addHostToShow activeId userId)

        inactiveId <- unwrapInsert (Shows.insertShow inactiveInsert {Shows.siStatus = Shows.Inactive})
        TRX.statement () (ShowHost.addHostToShow inactiveId userId)

      -- Filter by Active: should only see the active show
      activeResult <- runExceptT $ action Nothing Nothing (Just (Filter (Just Shows.Active))) Nothing Nothing

      -- Filter by Inactive: should only see the inactive show
      inactiveResult <- runExceptT $ action Nothing Nothing (Just (Filter (Just Shows.Inactive))) Nothing Nothing

      -- No filter: should see both
      allResult <- runExceptT $ action Nothing Nothing Nothing Nothing Nothing

      assert $ do
        activeVd <- assertRight activeResult
        inactiveVd <- assertRight inactiveResult
        allVd <- assertRight allResult

        length (svShows activeVd) === 1
        length (svShows inactiveVd) === 1
        length (svShows allVd) === 2
