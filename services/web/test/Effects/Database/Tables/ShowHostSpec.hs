{-# LANGUAGE OverloadedRecordDot #-}

module Effects.Database.Tables.ShowHostSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, assertSingleton)
import Test.Gen.EmailAddress ()
import Test.Gen.Password ()
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowHost" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: creates show-host relationship" $
          hedgehog . prop_insertSelect

      describe "Mutations" $ do
        runs 10 . it "removeShowHost: soft remove via left_at, excluded from getShowHosts" $
          hedgehog . prop_removeShowHost
        runs 10 . it "removeShowHost: second remove is no-op" $
          hedgehog . prop_removeShowHost_idempotent

      describe "Queries" $ do
        runs 10 . it "isUserHostOfShow: returns True for active host" $
          hedgehog . prop_isUserHostOfShow
        runs 10 . it "isUserHostOfShow: returns False after removal" $
          hedgehog . prop_isUserHostOfShow_afterRemoval
        runs 10 . it "multiple hosts per show with different roles" $
          hedgehog . prop_multipleHostsPerShow
        runs 10 . it "getShowHostsWithUsers: returns host with user metadata" $
          hedgehog . prop_getShowHostsWithUsers
        runs 10 . it "isUserHostOfShowSlug: returns True for active host by slug" $
          hedgehog . prop_isUserHostOfShowSlug
        runs 10 . it "addHostToShow: convenience wrapper adds host with Host role" $
          hedgehog . prop_addHostToShow

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert a show-host relationship and verify it can be retrieved.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        let hostRole = UUT.Host
        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId hostRole

        hosts <- TRX.statement () $ UUT.getShowHosts showId
        TRX.condemn
        pure (showId, userId, hostRole, hosts)

      assert $ do
        (expectedShowId, expectedUserId, expectedRole, hosts) <- assertRight result
        host <- assertSingleton hosts
        host.shmShowId === expectedShowId
        host.shmUserId === expectedUserId
        host.shmRole === expectedRole

--------------------------------------------------------------------------------
-- Mutations

-- | removeShowHost: sets left_at, getShowHosts excludes removed.
prop_removeShowHost :: TestDBConfig -> PropertyT IO ()
prop_removeShowHost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId UUT.Host

        hostsBefore <- TRX.statement () $ UUT.getShowHosts showId

        TRX.statement () $ UUT.removeShowHost showId userId

        hostsAfter <- TRX.statement () $ UUT.getShowHosts showId
        TRX.condemn
        pure (hostsBefore, hostsAfter)

      assert $ do
        (hostsBefore, hostsAfter) <- assertRight result
        length hostsBefore === 1
        length hostsAfter === 0
        pure ()

-- | removeShowHost: second remove is no-op (doesn't error).
prop_removeShowHost_idempotent :: TestDBConfig -> PropertyT IO ()
prop_removeShowHost_idempotent cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId UUT.Host

        -- Remove twice
        TRX.statement () $ UUT.removeShowHost showId userId
        TRX.statement () $ UUT.removeShowHost showId userId

        hostsAfter <- TRX.statement () $ UUT.getShowHosts showId
        TRX.condemn
        pure hostsAfter

      assert $ do
        hostsAfter <- assertRight result
        length hostsAfter === 0
        pure ()

--------------------------------------------------------------------------------
-- Queries

-- | isUserHostOfShow: returns True for an active host.
prop_isUserHostOfShow :: TestDBConfig -> PropertyT IO ()
prop_isUserHostOfShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId UUT.Host

        isHost <- TRX.statement () $ UUT.isUserHostOfShow userId showId
        TRX.condemn
        pure isHost

      assert $ do
        isHost <- assertRight result
        isHost === True
        pure ()

-- | isUserHostOfShow: returns False after removal.
prop_isUserHostOfShow_afterRemoval :: TestDBConfig -> PropertyT IO ()
prop_isUserHostOfShow_afterRemoval cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId UUT.Host
        TRX.statement () $ UUT.removeShowHost showId userId

        isHost <- TRX.statement () $ UUT.isUserHostOfShow userId showId
        TRX.condemn
        pure isHost

      assert $ do
        isHost <- assertRight result
        isHost === False
        pure ()

-- | Multiple hosts per show with different roles.
prop_multipleHostsPerShow :: TestDBConfig -> PropertyT IO ()
prop_multipleHostsPerShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId1 <- insertTestUser userWithMetadata1
        userId2 <- insertTestUser userWithMetadata2
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId1 UUT.Host
        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId2 UUT.CoHost

        hosts <- TRX.statement () $ UUT.getShowHosts showId
        TRX.condemn
        pure (userId1, userId2, hosts)

      assert $ do
        (userId1, userId2, hosts) <- assertRight result
        length hosts === 2
        let roles = map (.shmRole) hosts
        let userIds = map (.shmUserId) hosts
        elem UUT.Host roles === True
        elem UUT.CoHost roles === True
        elem userId1 userIds === True
        elem userId2 userIds === True
        pure ()

-- | getShowHostsWithUsers: returns host with user metadata (display name, etc.).
prop_getShowHostsWithUsers :: TestDBConfig -> PropertyT IO ()
prop_getShowHostsWithUsers cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId UUT.Host

        hostsWithUsers <- TRX.statement () $ UUT.getShowHostsWithUsers showId
        TRX.condemn
        pure (showId, userId, hostsWithUsers)

      assert $ do
        (expectedShowId, expectedUserId, hostsWithUsers) <- assertRight result
        host <- assertSingleton hostsWithUsers
        host.showId === expectedShowId
        host.userId === expectedUserId
        host.role === UUT.Host

-- | isUserHostOfShowSlug: returns True for active host by slug; False for non-host.
prop_isUserHostOfShowSlug :: TestDBConfig -> PropertyT IO ()
prop_isUserHostOfShowSlug cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId1 <- insertTestUser userWithMetadata1
        userId2 <- insertTestUser userWithMetadata2
        showId <- TRX.statement () $ Shows.insertShow showInsert

        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId1 UUT.Host

        let showSlug = Shows.siSlug showInsert
        isHost <- TRX.statement () $ UUT.isUserHostOfShowSlug userId1 showSlug
        isNotHost <- TRX.statement () $ UUT.isUserHostOfShowSlug userId2 showSlug
        TRX.condemn
        pure (isHost, isNotHost)

      assert $ do
        (isHost, isNotHost) <- assertRight result
        isHost === True
        isNotHost === False
        pure ()

-- | addHostToShow: convenience wrapper adds host, re-add after removal clears left_at.
prop_addHostToShow :: TestDBConfig -> PropertyT IO ()
prop_addHostToShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- TRX.statement () $ Shows.insertShow showInsert

        -- Add via convenience wrapper
        TRX.statement () $ UUT.addHostToShow showId userId
        hostsAfterAdd <- TRX.statement () $ UUT.getShowHosts showId

        -- Remove host
        TRX.statement () $ UUT.removeShowHost showId userId
        hostsAfterRemove <- TRX.statement () $ UUT.getShowHosts showId

        -- Re-add (upsert clears left_at)
        TRX.statement () $ UUT.addHostToShow showId userId
        hostsAfterReAdd <- TRX.statement () $ UUT.getShowHosts showId

        TRX.condemn
        pure (userId, hostsAfterAdd, hostsAfterRemove, hostsAfterReAdd)

      assert $ do
        (expectedUserId, hostsAfterAdd, hostsAfterRemove, hostsAfterReAdd) <- assertRight result
        -- After add: one host
        host <- assertSingleton hostsAfterAdd
        host.shmUserId === expectedUserId
        host.shmRole === UUT.Host
        -- After remove: zero hosts
        length hostsAfterRemove === 0
        -- After re-add: one host again
        reAddedHost <- assertSingleton hostsAfterReAdd
        reAddedHost.shmUserId === expectedUserId
        Hedgehog.assert (reAddedHost.shmLeftAt == Nothing)
