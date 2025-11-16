{-# LANGUAGE OverloadedRecordDot #-}

module Effects.Database.Tables.ShowHostSpec where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
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
      runs 5 . it "creates show-host relationship" $ hedgehog . prop_createShowHostRelationship
      runs 5 . it "retrieves hosts for show" $ hedgehog . prop_getHostsForShow

prop_createShowHostRelationship :: TestDBConfig -> PropertyT IO ()
prop_createShowHostRelationship cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert userWithMetadata.uwmiEmail userWithMetadata.uwmiPassword
        void $
          TRX.statement () $
            UserMetadata.insertUserMetadata $
              UserMetadata.ModelInsert
                userId
                userWithMetadata.uwmiDisplayName
                userWithMetadata.uwmiFullName
                userWithMetadata.uwmiAvatarUrl
                userWithMetadata.uwmiUserRole

        -- Create show
        showId <- TRX.statement () $ Shows.insertShow showInsert

        -- Create show-host relationship
        let hostRole = UUT.Host
            isPrimary = True
        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId hostRole isPrimary

        -- Get hosts for show
        hosts <- TRX.statement () $ UUT.getShowHosts showId
        pure (showId, userId, hostRole, isPrimary, hosts)

      assert $ do
        (expectedShowId, expectedUserId, expectedRole, expectedIsPrimary, hosts) <- assertRight result
        length hosts === 1
        let host = case hosts of
              (h : _) -> h
              [] -> error "Expected at least one host"
        host.shmId === expectedShowId
        host.shmUserId === expectedUserId
        host.shmRole === expectedRole
        host.shmIsPrimary === expectedIsPrimary

-- Test retrieving hosts for a show
prop_getHostsForShow :: TestDBConfig -> PropertyT IO ()
prop_getHostsForShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create user
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert userWithMetadata.uwmiEmail userWithMetadata.uwmiPassword
        void $
          TRX.statement () $
            UserMetadata.insertUserMetadata $
              UserMetadata.ModelInsert
                userId
                userWithMetadata.uwmiDisplayName
                userWithMetadata.uwmiFullName
                userWithMetadata.uwmiAvatarUrl
                userWithMetadata.uwmiUserRole

        -- Create show
        showId <- TRX.statement () $ Shows.insertShow showInsert

        -- Create show-host relationship
        let hostRole = UUT.Host
            isPrimary = True
        TRX.statement () $ UUT.insertShowHost $ UUT.Insert showId userId hostRole isPrimary

        -- Get hosts for show
        hosts <- TRX.statement () $ UUT.getShowHosts showId
        pure (showId, userId, hostRole, hosts)

      assert $ do
        (expectedShowId, expectedUserId, expectedRole, hosts) <- assertRight result
        length hosts === 1
        let host = case hosts of
              (h : _) -> h
              [] -> error "Expected at least one host"
        host.shmId === expectedShowId
        host.shmUserId === expectedUserId
        host.shmRole === expectedRole
