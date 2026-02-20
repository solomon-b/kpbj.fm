{-# LANGUAGE OverloadedRecordDot #-}

-- | Integration tests for "App.Handler.Combinators".
--
-- Each combinator is tested against a real test database to verify that role
-- checks and suspension checks behave correctly end-to-end.
module App.Handler.CombinatorsSpec (spec) where

--------------------------------------------------------------------------------

import App.Handler.Combinators
  ( requireAdminNotSuspended,
    requireHostNotSuspended,
    requireJust,
    requireRight,
    requireShowHostOrStaff,
    requireStaffNotSuspended,
  )
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "App.Handler.Combinators" $ do
      describe "requireHostNotSuspended" $ do
        it "succeeds for a host who is not suspended" test_hostNotSuspended_succeedsForHost
        it "succeeds for a staff user (higher role)" test_hostNotSuspended_succeedsForStaff
        it "succeeds for an admin user (higher role)" test_hostNotSuspended_succeedsForAdmin
        it "fails for a user-role account (insufficient role)" test_hostNotSuspended_failsForUser
        it "fails for a suspended host" test_hostNotSuspended_failsForSuspendedHost

      describe "requireStaffNotSuspended" $ do
        it "succeeds for a staff user who is not suspended" test_staffNotSuspended_succeedsForStaff
        it "succeeds for an admin user (higher role)" test_staffNotSuspended_succeedsForAdmin
        it "fails for a host-role account (insufficient role)" test_staffNotSuspended_failsForHost
        it "fails for a suspended staff user" test_staffNotSuspended_failsForSuspendedStaff

      describe "requireAdminNotSuspended" $ do
        it "succeeds for an admin who is not suspended" test_adminNotSuspended_succeedsForAdmin
        it "fails for a staff-role account (insufficient role)" test_adminNotSuspended_failsForStaff
        it "fails for a suspended admin" test_adminNotSuspended_failsForSuspendedAdmin

      describe "requireShowHostOrStaff" $ do
        it "succeeds when the user is a host of the show and not suspended" test_showHostOrStaff_succeedsForShowHost
        it "succeeds for a staff user who is not a host (staff bypass)" test_showHostOrStaff_succeedsForStaff
        it "fails when the user is a host of a different show" test_showHostOrStaff_failsForUnrelatedHost
        it "fails when the user is a host of the show but suspended" test_showHostOrStaff_failsForSuspendedHost

      describe "requireJust" $ do
        it "returns the inner value when given Just" test_requireJust_succeedsForJust
        it "fails with ValidationError when given Nothing" test_requireJust_failsForNothing

      describe "requireRight" $ do
        it "returns the inner value when given Right" test_requireRight_succeedsForRight
        it "fails with ValidationError when given Left" test_requireRight_failsForLeft

--------------------------------------------------------------------------------
-- requireHostNotSuspended

-- | A host who is not suspended should pass the check.
test_hostNotSuspended_succeedsForHost :: TestDBConfig -> IO ()
test_hostNotSuspended_succeedsForHost cfg = do
  userInsert <- mkUserInsert "comb-host-ok" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireHostNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A staff user satisfies the host-or-higher check.
test_hostNotSuspended_succeedsForStaff :: TestDBConfig -> IO ()
test_hostNotSuspended_succeedsForStaff cfg = do
  userInsert <- mkUserInsert "comb-host-staff-ok" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireHostNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | An admin user satisfies the host-or-higher check.
test_hostNotSuspended_succeedsForAdmin :: TestDBConfig -> IO ()
test_hostNotSuspended_succeedsForAdmin cfg = do
  userInsert <- mkUserInsert "comb-host-admin-ok" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireHostNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A plain user role is below host; the check should throw NotAuthorized.
test_hostNotSuspended_failsForUser :: TestDBConfig -> IO ()
test_hostNotSuspended_failsForUser cfg = do
  userInsert <- mkUserInsert "comb-host-user-fail" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireHostNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

-- | A host who has been suspended should fail the check even though their role
-- is host-or-higher.
test_hostNotSuspended_failsForSuspendedHost :: TestDBConfig -> IO ()
test_hostNotSuspended_failsForSuspendedHost cfg = do
  userInsert <- mkUserInsert "comb-host-suspended-fail" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _ <- TRX.statement () (UserMetadata.suspendUser userModel.mId "test suspension")
      TRX.statement () (UserMetadata.getUserMetadata userModel.mId)
        >>= maybe (error "metadata not found after suspension") pure
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ requireHostNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

--------------------------------------------------------------------------------
-- requireStaffNotSuspended

-- | A staff user who is not suspended should pass the check.
test_staffNotSuspended_succeedsForStaff :: TestDBConfig -> IO ()
test_staffNotSuspended_succeedsForStaff cfg = do
  userInsert <- mkUserInsert "comb-staff-ok" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireStaffNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | An admin user satisfies the staff-or-higher check.
test_staffNotSuspended_succeedsForAdmin :: TestDBConfig -> IO ()
test_staffNotSuspended_succeedsForAdmin cfg = do
  userInsert <- mkUserInsert "comb-staff-admin-ok" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireStaffNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A host role is below staff; the check should throw NotAuthorized.
test_staffNotSuspended_failsForHost :: TestDBConfig -> IO ()
test_staffNotSuspended_failsForHost cfg = do
  userInsert <- mkUserInsert "comb-staff-host-fail" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireStaffNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

-- | A staff user who has been suspended should fail the check.
test_staffNotSuspended_failsForSuspendedStaff :: TestDBConfig -> IO ()
test_staffNotSuspended_failsForSuspendedStaff cfg = do
  userInsert <- mkUserInsert "comb-staff-suspended-fail" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _ <- TRX.statement () (UserMetadata.suspendUser userModel.mId "test suspension")
      TRX.statement () (UserMetadata.getUserMetadata userModel.mId)
        >>= maybe (error "metadata not found after suspension") pure
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ requireStaffNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

--------------------------------------------------------------------------------
-- requireAdminNotSuspended

-- | An admin who is not suspended should pass the check.
test_adminNotSuspended_succeedsForAdmin :: TestDBConfig -> IO ()
test_adminNotSuspended_succeedsForAdmin cfg = do
  userInsert <- mkUserInsert "comb-admin-ok" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireAdminNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A staff role is below admin; the check should throw NotAuthorized.
test_adminNotSuspended_failsForStaff :: TestDBConfig -> IO ()
test_adminNotSuspended_failsForStaff cfg = do
  userInsert <- mkUserInsert "comb-admin-staff-fail" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireAdminNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

-- | A suspended admin should fail the check even though they have admin role.
test_adminNotSuspended_failsForSuspendedAdmin :: TestDBConfig -> IO ()
test_adminNotSuspended_failsForSuspendedAdmin cfg = do
  userInsert <- mkUserInsert "comb-admin-suspended-fail" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _ <- TRX.statement () (UserMetadata.suspendUser userModel.mId "test suspension")
      TRX.statement () (UserMetadata.getUserMetadata userModel.mId)
        >>= maybe (error "metadata not found after suspension") pure
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ requireAdminNotSuspended "msg" userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

--------------------------------------------------------------------------------
-- requireShowHostOrStaff

-- | A show host who is not suspended can access their own show.
test_showHostOrStaff_succeedsForShowHost :: TestDBConfig -> IO ()
test_showHostOrStaff_succeedsForShowHost cfg = do
  userInsert <- mkUserInsert "comb-shohost-ok" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Comb Show Host OK" (Slug "comb-showhost-ok") Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireShowHostOrStaff userModel.mId showModel.slug userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A staff user can access any show regardless of host membership.
test_showHostOrStaff_succeedsForStaff :: TestDBConfig -> IO ()
test_showHostOrStaff_succeedsForStaff cfg = do
  userInsert <- mkUserInsert "comb-shostaff-ok" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      -- The staff user is NOT added as a host of this show
      let showInsert = Shows.Insert "Comb Show Staff OK" (Slug "comb-showstaff-ok") Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireShowHostOrStaff userModel.mId showModel.slug userMetaModel

    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | A host of one show cannot access a different show they are not hosting.
test_showHostOrStaff_failsForUnrelatedHost :: TestDBConfig -> IO ()
test_showHostOrStaff_failsForUnrelatedHost cfg = do
  userInsert <- mkUserInsert "comb-shohost-other-fail" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      -- Create the user's show and add them as host
      let ownShowInsert = Shows.Insert "Comb Host Own Show" (Slug "comb-host-own-show") Nothing Nothing Shows.Active
      (ownShowId, _) <- insertTestShowWithSchedule ownShowInsert defaultScheduleInsert
      addTestShowHost ownShowId userModel.mId
      -- Create a different show they are NOT a host of
      let otherShowInsert = Shows.Insert "Comb Host Other Show" (Slug "comb-host-other-show") Nothing Nothing Shows.Active
      (otherId, _) <- insertTestShowWithSchedule otherShowInsert defaultScheduleInsert
      otherShowModel <-
        TRX.statement () (Shows.getShowById otherId)
          >>= maybe (error "other show not found") pure
      pure (userModel, userMetaModel, otherShowModel)
    (userModel, userMetaModel, otherShowModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireShowHostOrStaff userModel.mId otherShowModel.slug userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

-- | A suspended host cannot access their own show.
test_showHostOrStaff_failsForSuspendedHost :: TestDBConfig -> IO ()
test_showHostOrStaff_failsForSuspendedHost cfg = do
  userInsert <- mkUserInsert "comb-shohost-suspended-fail" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Comb Suspended Host Show" (Slug "comb-suspended-host-show") Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId
      -- Suspend the host after assigning them
      _ <- TRX.statement () (UserMetadata.suspendUser userModel.mId "test suspension")
      refreshedMeta <-
        TRX.statement () (UserMetadata.getUserMetadata userModel.mId)
          >>= maybe (error "metadata not found after suspension") pure
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userModel, refreshedMeta, showModel)
    (userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ requireShowHostOrStaff userModel.mId showModel.slug userMetaModel

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right () -> expectationFailure "Expected Left NotAuthorized but got Right"

--------------------------------------------------------------------------------
-- requireJust

-- | Providing a Just value returns the unwrapped value.
test_requireJust_succeedsForJust :: TestDBConfig -> IO ()
test_requireJust_succeedsForJust cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireJust "error msg" (Just (42 :: Int))

    liftIO $ case result of
      Right 42 -> pure ()
      Right v -> expectationFailure $ "Expected Right 42 but got Right " <> show v
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err

-- | Providing Nothing throws a ValidationError.
test_requireJust_failsForNothing :: TestDBConfig -> IO ()
test_requireJust_failsForNothing cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireJust "error msg" (Nothing :: Maybe Int)

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

--------------------------------------------------------------------------------
-- requireRight

-- | Providing a Right value returns the unwrapped value.
test_requireRight_succeedsForRight :: TestDBConfig -> IO ()
test_requireRight_succeedsForRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireRight id (Right (99 :: Int) :: Either Text Int)

    liftIO $ case result of
      Right 99 -> pure ()
      Right v -> expectationFailure $ "Expected Right 99 but got Right " <> show v
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err

-- | Providing a Left value throws a ValidationError carrying the converted message.
test_requireRight_failsForLeft :: TestDBConfig -> IO ()
test_requireRight_failsForLeft cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireRight id (Left "something went wrong" :: Either Text Int)

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"
