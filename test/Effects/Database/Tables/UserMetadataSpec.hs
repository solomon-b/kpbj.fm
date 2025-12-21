{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.UserMetadataSpec where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Int (Int64)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UUT
import Hasql.Interpolate (OneColumn (getOneColumn), OneRow (OneRow), interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
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

-- Simplified test that works end-to-end
prop_insertUserWithMetadata :: TestDBConfig -> PropertyT IO ()
prop_insertUserWithMetadata cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata)
        user <- TRX.statement () (User.getUser userId)
        metadata <- TRX.statement () (UUT.getUserMetadata userId)
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
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata)

        -- Verify user exists before deletion
        metadataBeforeDelete <- TRX.statement () (UUT.getUserMetadata userId)

        -- Soft delete the user
        deletedUserId <- TRX.statement () (UUT.softDeleteUser userId)

        -- Verify user is filtered out after deletion
        metadataAfterDelete <- TRX.statement () (UUT.getUserMetadata userId)

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
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata)

        -- First soft delete
        firstDelete <- TRX.statement () (UUT.softDeleteUser userId)

        -- Second soft delete (should be idempotent)
        secondDelete <- TRX.statement () (UUT.softDeleteUser userId)

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
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata)

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
        _metadataId <- TRX.statement () $ UUT.insertUserMetadata $ UUT.Insert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata) (UUT.uwmiColorScheme userWithMetadata)

        -- Get users before deletion
        usersBeforeDelete <- TRX.statement () (UUT.getAllUsersWithPagination 100 0)

        -- Soft delete the user
        void $ TRX.statement () (UUT.softDeleteUser userId)

        -- Get users after deletion
        usersAfterDelete <- TRX.statement () (UUT.getAllUsersWithPagination 100 0)

        pure (userId, usersBeforeDelete, usersAfterDelete)

      assert $ do
        (userId, usersBeforeDelete, usersAfterDelete) <- assertRight result
        -- User should be in the list before deletion
        let userIdsBeforeDelete = map UUT.uwmUserId usersBeforeDelete
        elem userId userIdsBeforeDelete === True
        -- User should NOT be in the list after deletion
        let userIdsAfterDelete = map UUT.uwmUserId usersAfterDelete
        elem userId userIdsAfterDelete === False
