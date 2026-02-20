{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Handler (UsersListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Users.Get.Handler" $ do
      describe "action" $ do
        it "succeeds with only the viewer in the DB" test_singleViewer
        it "returns users after insert" test_returnsUsers
        it "defaults to page 1" test_defaultsToPageOne
        it "filters by role" test_roleFilter
        it "returns Right on any valid request" test_returnsRight

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | With only the viewer in the DB, action succeeds and returns page 1.
test_singleViewer :: TestDBConfig -> IO ()
test_singleViewer cfg = do
  viewerInsert <- mkUserInsert "usr-get-viewer-empty" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta Nothing Nothing Nothing Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        -- The viewer themselves will appear in the list, so just check it succeeds
        vd.ulvPage `shouldBe` 1
        vd.ulvIsAppendRequest `shouldBe` False

-- | Inserting users causes them to appear in the list.
test_returnsUsers :: TestDBConfig -> IO ()
test_returnsUsers cfg = do
  viewerInsert <- mkUserInsert "usr-get-viewer-list" UserMetadata.Admin
  targetInsert <- mkUserInsert "usr-get-target-list" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      _ <- insertTestUser targetInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta Nothing Nothing Nothing Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length vd.ulvUsers `shouldSatisfy` (>= 2)

-- | No page param defaults to page 1.
test_defaultsToPageOne :: TestDBConfig -> IO ()
test_defaultsToPageOne cfg = do
  viewerInsert <- mkUserInsert "usr-get-viewer-page" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta Nothing Nothing Nothing Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.ulvPage `shouldBe` (1 :: Int64)

-- | Role filter limits results to matching role only.
test_roleFilter :: TestDBConfig -> IO ()
test_roleFilter cfg = do
  viewerInsert <- mkUserInsert "usr-get-viewer-role" UserMetadata.Admin
  hostInsert <- mkUserInsert "usr-get-host-role" UserMetadata.Host
  userInsert <- mkUserInsert "usr-get-user-role" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      _ <- insertTestUser hostInsert
      _ <- insertTestUser userInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    let hostFilter = Just (Filter (Just UserMetadata.Host))
    result <- runExceptT $ action viewerModel viewerMeta Nothing Nothing hostFilter Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        vd.ulvRoleFilter `shouldBe` Just UserMetadata.Host
        -- All returned users should be hosts
        all (\u -> u.uwmUserRole == UserMetadata.Host) vd.ulvUsers `shouldBe` True

-- | Action always returns Right for valid inputs.
test_returnsRight :: TestDBConfig -> IO ()
test_returnsRight cfg = do
  viewerInsert <- mkUserInsert "usr-get-viewer-right" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta Nothing Nothing Nothing (Just (Filter (Just JoinDateNewest))) IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
