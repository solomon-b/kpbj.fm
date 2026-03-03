{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.UserMetadataSpec where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Int (Int64)
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.FullName ()
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UUT
import Hasql.Interpolate (OneColumn (getOneColumn), OneRow (OneRow), interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton)
import Test.Gen.EmailAddress ()
import Test.Gen.Password ()
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.UserMetadata" $ do
      runs 10 . it "schema validation: insert user with metadata" $ hedgehog . prop_insertUserWithMetadata
      runs 10 . it "soft delete: sets deleted_at and filters user from queries" $ hedgehog . prop_softDeleteFiltersUser
      runs 10 . it "soft delete: is idempotent (returns Nothing on second delete)" $ hedgehog . prop_softDeleteIdempotent
      runs 10 . it "soft delete: invalidates all user sessions" $ hedgehog . prop_softDeleteInvalidatesSessions
      runs 10 . it "soft delete: deleted users excluded from getAllUsersWithPagination" $ hedgehog . prop_deletedUsersExcludedFromPagination

      describe "Mutations" $ do
        runs 10 . it "updateUserRole: changes user role" $ hedgehog . prop_updateUserRole
        runs 10 . it "updateUserMetadata: updates display name" $ hedgehog . prop_updateUserMetadata
        runs 10 . it "suspendUser: sets suspension and invalidates sessions" $ hedgehog . prop_suspendUser
        runs 10 . it "unsuspendUser: clears suspension status" $ hedgehog . prop_unsuspendUser

      describe "Queries" $ do
        runs 10 . it "getUsersByRole: filters by role" $ hedgehog . prop_getUsersByRole
        runs 10 . it "searchUsers: finds by display name" $ hedgehog . prop_searchUsers
        runs 10 . it "getUserWithMetadataById: returns joined user data" $ hedgehog . prop_getUserWithMetadataById

-- Simplified test that works end-to-end
prop_insertUserWithMetadata :: TestDBConfig -> PropertyT IO ()
prop_insertUserWithMetadata cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata) (UUT.uwmiTheme userWithMetadata)
        user <- TRX.statement () (User.getUser userId)
        metadata <- TRX.statement () (UUT.getUserMetadata userId)
        TRX.condemn
        pure (userId, metadataId, user, metadata)

      assert $ do
        (userId, _metadataId, mUser, mMetadata) <- assertRight result
        user <- assertJust mUser
        metadata <- assertJust mMetadata
        User.mEmail user === UUT.uwmiEmail userWithMetadata
        UUT.mUserId metadata === userId
        UUT.mDisplayName metadata === UUT.uwmiDisplayName userWithMetadata
        UUT.mFullName metadata === UUT.uwmiFullName userWithMetadata
        UUT.mAvatarUrl metadata === UUT.uwmiAvatarUrl userWithMetadata
        UUT.mUserRole metadata === UUT.uwmiUserRole userWithMetadata

-- | Test that soft delete filters user from getUserMetadata queries
prop_softDeleteFiltersUser :: TestDBConfig -> PropertyT IO ()
prop_softDeleteFiltersUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Insert user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata) (UUT.uwmiTheme userWithMetadata)

        -- Verify user exists before deletion
        metadataBeforeDelete <- TRX.statement () (UUT.getUserMetadata userId)

        -- Soft delete the user
        deletedUserId <- TRX.statement () (UUT.softDeleteUser userId)

        -- Verify user is filtered out after deletion
        metadataAfterDelete <- TRX.statement () (UUT.getUserMetadata userId)

        TRX.condemn
        pure (userId, metadataBeforeDelete, deletedUserId, metadataAfterDelete)

      assert $ do
        (userId, metadataBeforeDelete, deletedUserId, metadataAfterDelete) <- assertRight result
        -- User should exist before deletion
        void $ assertJust metadataBeforeDelete
        -- Soft delete should return the deleted user ID
        deletedUserId === Just userId
        -- User should be filtered out after deletion
        metadataAfterDelete === Nothing

-- | Test that soft delete is idempotent (deleting twice returns Nothing second time)
prop_softDeleteIdempotent :: TestDBConfig -> PropertyT IO ()
prop_softDeleteIdempotent cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Insert user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata) (UUT.uwmiTheme userWithMetadata)

        -- First soft delete
        firstDelete <- TRX.statement () (UUT.softDeleteUser userId)

        -- Second soft delete (should be idempotent)
        secondDelete <- TRX.statement () (UUT.softDeleteUser userId)

        TRX.condemn
        pure (userId, firstDelete, secondDelete)

      assert $ do
        (userId, firstDelete, secondDelete) <- assertRight result
        -- First delete should succeed and return user ID
        firstDelete === Just userId
        -- Second delete should return Nothing (already deleted)
        secondDelete === Nothing

-- | Test that soft delete invalidates all user sessions
prop_softDeleteInvalidatesSessions :: TestDBConfig -> PropertyT IO ()
prop_softDeleteInvalidatesSessions cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Insert user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata) (UUT.uwmiTheme userWithMetadata)

        -- Create a session for the user (direct SQL insert)
        TRX.statement () $
          interp @()
            False
            [sql|
          INSERT INTO server_sessions (id, user_id, expires_at)
          VALUES (gen_random_uuid(), #{userId}, NOW() + INTERVAL '1 day')
        |]

        -- Count sessions before deletion
        mSessionsBeforeDelete <-
          TRX.statement () $
            interp
              False
              [sql|
          SELECT COUNT(*)::bigint FROM server_sessions WHERE user_id = #{userId}
        |]
        let sessionsBeforeDelete = maybe 0 getOneColumn mSessionsBeforeDelete

        -- Soft delete the user
        void $ TRX.statement () (UUT.softDeleteUser userId)

        -- Count sessions after deletion
        mSessionsAfterDelete <-
          TRX.statement () $
            interp
              False
              [sql|
          SELECT COUNT(*)::bigint FROM server_sessions WHERE user_id = #{userId}
        |]
        let sessionsAfterDelete = maybe 0 getOneColumn mSessionsAfterDelete

        TRX.condemn
        pure (sessionsBeforeDelete, sessionsAfterDelete)

      assert $ do
        (sessionsBeforeDelete, sessionsAfterDelete) <- assertRight result
        -- Should have at least one session before deletion
        sessionsBeforeDelete === (1 :: Int64)
        -- Should have zero sessions after deletion
        sessionsAfterDelete === (0 :: Int64)

-- | Test that deleted users are excluded from getAllUsersWithPagination
prop_deletedUsersExcludedFromPagination :: TestDBConfig -> PropertyT IO ()
prop_deletedUsersExcludedFromPagination cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Insert user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata) (UUT.uwmiTheme userWithMetadata)

        -- Get users before deletion
        usersBeforeDelete <- TRX.statement () (UUT.getAllUsersWithPagination 100 0)

        -- Soft delete the user
        void $ TRX.statement () (UUT.softDeleteUser userId)

        -- Get users after deletion
        usersAfterDelete <- TRX.statement () (UUT.getAllUsersWithPagination 100 0)

        TRX.condemn
        pure (userId, usersBeforeDelete, usersAfterDelete)

      assert $ do
        (userId, usersBeforeDelete, usersAfterDelete) <- assertRight result
        -- User should be in the list before deletion
        let userIdsBeforeDelete = map UUT.uwmUserId usersBeforeDelete
        elem userId userIdsBeforeDelete === True
        -- User should NOT be in the list after deletion
        let userIdsAfterDelete = map UUT.uwmUserId usersAfterDelete
        elem userId userIdsAfterDelete === False

-- | Test that updateUserRole changes the user's role
prop_updateUserRole :: TestDBConfig -> PropertyT IO ()
prop_updateUserRole cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Update role to Admin
        mUpdatedId <- TRX.statement () (UUT.updateUserRole userId UUT.Admin)

        -- Read back the metadata
        mMetadata <- TRX.statement () (UUT.getUserMetadata userId)

        TRX.condemn
        pure (mUpdatedId, mMetadata)

      assert $ do
        (mUpdatedId, mMetadata) <- assertRight result
        _ <- assertJust mUpdatedId
        metadata <- assertJust mMetadata
        UUT.mUserRole metadata === UUT.Admin

-- | Test that updateUserMetadata updates the display name (and preserves other fields)
prop_updateUserMetadata :: TestDBConfig -> PropertyT IO ()
prop_updateUserMetadata cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Get the metadata to obtain its ID
        mMetadataBefore <- TRX.statement () (UUT.getUserMetadata userId)

        updateResult <- case mMetadataBefore of
          Nothing -> pure (Nothing, Nothing)
          Just metadataBefore -> do
            let newDisplayName = mkDisplayNameUnsafe "Updated Test Name"
            let update =
                  UUT.Update
                    { UUT.uDisplayName = Just newDisplayName,
                      UUT.uFullName = Nothing,
                      UUT.uAvatarUrl = Nothing,
                      UUT.uColorScheme = Nothing,
                      UUT.uTheme = Nothing
                    }
            mUpdatedId <- TRX.statement () (UUT.updateUserMetadata (UUT.mId metadataBefore) update)

            -- Read back the metadata
            mMetadataAfter <- TRX.statement () (UUT.getUserMetadata userId)
            pure (mUpdatedId, mMetadataAfter)
        TRX.condemn
        pure updateResult

      assert $ do
        (mUpdatedId, mMetadataAfter) <- assertRight result
        _ <- assertJust mUpdatedId
        metadataAfter <- assertJust mMetadataAfter
        -- Display name should be updated
        UUT.mDisplayName metadataAfter === mkDisplayNameUnsafe "Updated Test Name"
        -- Full name should be preserved (COALESCE keeps original)
        UUT.mFullName metadataAfter === UUT.uwmiFullName userWithMetadata

-- | Test that suspendUser sets suspension status and invalidates all sessions
prop_suspendUser :: TestDBConfig -> PropertyT IO ()
prop_suspendUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Create a session for the user
        TRX.statement () $
          interp @()
            False
            [sql|
          INSERT INTO server_sessions (id, user_id, expires_at)
          VALUES (gen_random_uuid(), #{userId}, NOW() + INTERVAL '1 day')
        |]

        -- Count sessions before suspension
        mSessionsBefore <-
          TRX.statement () $
            interp
              False
              [sql|
          SELECT COUNT(*)::bigint FROM server_sessions WHERE user_id = #{userId}
        |]
        let sessionsBefore = maybe 0 getOneColumn mSessionsBefore

        -- Suspend the user
        mSuspendedId <- TRX.statement () (UUT.suspendUser userId "Test suspension reason")

        -- Check suspension status
        mMetadata <- TRX.statement () (UUT.getUserMetadata userId)

        -- Count sessions after suspension
        mSessionsAfter <-
          TRX.statement () $
            interp
              False
              [sql|
          SELECT COUNT(*)::bigint FROM server_sessions WHERE user_id = #{userId}
        |]
        let sessionsAfter = maybe 0 getOneColumn mSessionsAfter

        TRX.condemn
        pure (userId, mSuspendedId, mMetadata, sessionsBefore, sessionsAfter)

      assert $ do
        (userId, mSuspendedId, mMetadata, sessionsBefore, sessionsAfter) <- assertRight result
        -- Suspend should return the user ID
        mSuspendedId === Just userId
        -- User metadata should still be accessible (not deleted)
        metadata <- assertJust mMetadata
        -- Suspension status should be Suspended
        UUT.mSuspensionStatus metadata === UUT.Suspended
        -- Should have had one session before
        sessionsBefore === (1 :: Int64)
        -- Sessions should be invalidated
        sessionsAfter === (0 :: Int64)

-- | Test that unsuspendUser clears suspension status
prop_unsuspendUser :: TestDBConfig -> PropertyT IO ()
prop_unsuspendUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Suspend the user
        _ <- TRX.statement () (UUT.suspendUser userId "Test reason")

        -- Verify suspended
        mSuspended <- TRX.statement () (UUT.getUserMetadata userId)

        -- Unsuspend the user
        mUnsuspendedId <- TRX.statement () (UUT.unsuspendUser userId)

        -- Verify unsuspended
        mAfterUnsuspend <- TRX.statement () (UUT.getUserMetadata userId)

        -- Unsuspending a non-suspended user returns Nothing
        mSecondUnsuspend <- TRX.statement () (UUT.unsuspendUser userId)

        TRX.condemn
        pure (userId, mSuspended, mUnsuspendedId, mAfterUnsuspend, mSecondUnsuspend)

      assert $ do
        (userId, mSuspended, mUnsuspendedId, mAfterUnsuspend, mSecondUnsuspend) <- assertRight result
        -- Should have been suspended
        suspended <- assertJust mSuspended
        UUT.mSuspensionStatus suspended === UUT.Suspended
        -- Unsuspend should return the user ID
        unsuspendedId <- assertJust mUnsuspendedId
        unsuspendedId === userId
        -- Should be NotSuspended after unsuspend
        afterUnsuspend <- assertJust mAfterUnsuspend
        UUT.mSuspensionStatus afterUnsuspend === UUT.NotSuspended
        -- Second unsuspend returns Nothing
        assertNothing mSecondUnsuspend

-- | Test that getUsersByRole filters by role
prop_getUsersByRole :: TestDBConfig -> PropertyT IO ()
prop_getUsersByRole cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create an Admin user
        userId1 <- insertTestUser userWithMetadata1
        _ <- TRX.statement () (UUT.updateUserRole userId1 UUT.Admin)

        -- Create a regular User
        userId2 <- insertTestUser userWithMetadata2
        _ <- TRX.statement () (UUT.updateUserRole userId2 UUT.User)

        -- Filter by [Admin] should return only admin
        admins <- TRX.statement () (UUT.getUsersByRole [UUT.Admin] (Limit 100) (Offset 0) JoinDateNewest)

        -- Empty list returns all users
        allUsers <- TRX.statement () (UUT.getUsersByRole [] (Limit 100) (Offset 0) JoinDateNewest)

        -- Limit/Offset respected
        limited <- TRX.statement () (UUT.getUsersByRole [] (Limit 1) (Offset 0) JoinDateNewest)

        TRX.condemn
        pure (userId1, userId2, admins, allUsers, limited)

      assert $ do
        (userId1, userId2, admins, allUsers, limited) <- assertRight result
        -- Only admin returned for [Admin] filter
        admin <- assertSingleton admins
        UUT.uwmUserId admin === userId1
        -- All users returned for empty filter
        let allUserIds = map UUT.uwmUserId allUsers
        elem userId1 allUserIds === True
        elem userId2 allUserIds === True
        -- Limit respected
        length limited === 1
        pure ()

-- | Test that searchUsers finds by display name
prop_searchUsers :: TestDBConfig -> PropertyT IO ()
prop_searchUsers cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Update display name to something searchable
        mMetadata <- TRX.statement () (UUT.getUserMetadata userId)
        case mMetadata of
          Nothing -> pure (userId, [], [])
          Just metadata -> do
            let knownName = mkDisplayNameUnsafe "UniqueSearchableTestName"
            let update = UUT.Update {UUT.uDisplayName = Just knownName, UUT.uFullName = Nothing, UUT.uAvatarUrl = Nothing, UUT.uColorScheme = Nothing, UUT.uTheme = Nothing}
            _ <- TRX.statement () (UUT.updateUserMetadata (UUT.mId metadata) update)

            -- Search matching
            found <- TRX.statement () (UUT.searchUsers "UniqueSearchableTestName" [] (Limit 10) (Offset 0) JoinDateNewest)

            -- Search non-matching
            notFound <- TRX.statement () (UUT.searchUsers "ZZNoSuchUserExists" [] (Limit 10) (Offset 0) JoinDateNewest)

            TRX.condemn
            pure (userId, found, notFound)

      assert $ do
        (userId, found, notFound) <- assertRight result
        foundUser <- assertSingleton found
        UUT.uwmUserId foundUser === userId
        length notFound === 0
        pure ()

-- | Test that getUserWithMetadataById returns joined user data
prop_getUserWithMetadataById :: TestDBConfig -> PropertyT IO ()
prop_getUserWithMetadataById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Get by ID
        mFound <- TRX.statement () (UUT.getUserWithMetadataById userId)

        -- Non-existent ID returns Nothing
        mNotFound <- TRX.statement () (UUT.getUserWithMetadataById (User.Id 999999))

        TRX.condemn
        pure (userId, userWithMetadata, mFound, mNotFound)

      assert $ do
        (userId, uwmi, mFound, mNotFound) <- assertRight result
        found <- assertJust mFound
        UUT.uwmUserId found === userId
        UUT.uwmEmail found === UUT.uwmiEmail uwmi
        UUT.uwmDisplayName found === UUT.uwmiDisplayName uwmi
        assertNothing mNotFound
        pure ()
