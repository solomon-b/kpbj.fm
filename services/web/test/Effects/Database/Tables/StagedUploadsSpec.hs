{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.StagedUploadsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StagedUploads qualified as UUT
import Effects.StagedUploads (generateSecureToken)
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.StagedUploads (completeInsert, partialStagedUploadInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.StagedUploads" $ do
      runs 10 . it "lens law: insert-getByToken returns inserted upload" $ hedgehog . prop_insertGetByToken
      runs 10 . it "lens law: getByToken returns Nothing for non-existent token" $ hedgehog . prop_getByTokenNotFound
      runs 5 . it "claim: claimUpload marks upload as claimed" $ hedgehog . prop_claimUpload
      runs 5 . it "claim: claimUpload fails for wrong user" $ hedgehog . prop_claimUploadWrongUser
      runs 5 . it "claim: claimUpload fails for already claimed upload" $ hedgehog . prop_claimUploadAlreadyClaimed
      runs 5 . it "delete: deleteById removes upload" $ hedgehog . prop_deleteById
      runs 5 . it "expiry: getExpiredUploads returns only expired pending uploads" $ hedgehog . prop_getExpiredUploads
      runs 5 . it "expiry: getExpiredUploads returns empty when no uploads are expired" $ hedgehog . prop_getExpiredUploads_empty

--------------------------------------------------------------------------------
-- Property Tests

-- | Lens Law: insert then getByToken returns what we inserted
prop_insertGetByToken :: TestDBConfig -> PropertyT IO ()
prop_insertGetByToken cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      -- Generate token outside the transaction (needs MonadIO)
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user
        userId <- insertTestUser userWithMetadata

        -- Complete the insert with user ID and token
        let stagedUploadInsert = completeInsert userId token partialUpload
        mUploadId <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Fetch by token
        selected <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure (mUploadId, stagedUploadInsert, selected)

      assert $ do
        (mUploadId, insertData, mSelected) <- assertRight result
        uploadId <- assertJust mUploadId
        selected <- assertJust mSelected
        -- Verify the data matches
        UUT.id selected === uploadId
        UUT.token selected === UUT.siToken insertData
        UUT.originalName selected === UUT.siOriginalName insertData
        UUT.storagePath selected === UUT.siStoragePath insertData
        UUT.mimeType selected === UUT.siMimeType insertData
        UUT.fileSize selected === UUT.siFileSize insertData
        UUT.uploadType selected === UUT.siUploadType insertData
        UUT.status selected === UUT.Pending

-- | getByToken returns Nothing for non-existent token
prop_getByTokenNotFound :: TestDBConfig -> PropertyT IO ()
prop_getByTokenNotFound cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        TRX.statement () (UUT.getByToken (UUT.Token "nonexistent_token_12345678"))

      assert $ do
        mSelected <- assertRight result
        assertNothing mSelected

-- | claimUpload marks upload as claimed and returns the model
prop_claimUpload :: TestDBConfig -> PropertyT IO ()
prop_claimUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user
        userId <- insertTestUser userWithMetadata

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Claim it
        claimed <- TRX.statement () (UUT.claimUpload token userId)

        -- Verify status changed
        afterClaim <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure (claimed, afterClaim)

      assert $ do
        (mClaimed, mAfterClaim) <- assertRight result
        claimed <- assertJust mClaimed
        afterClaim <- assertJust mAfterClaim
        -- Claimed should return the model
        UUT.status claimed === UUT.Claimed
        -- After claim, status should be Claimed
        UUT.status afterClaim === UUT.Claimed

-- | claimUpload fails for wrong user
prop_claimUploadWrongUser :: TestDBConfig -> PropertyT IO ()
prop_claimUploadWrongUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create two users
        userId1 <- insertTestUser userWithMetadata1
        userId2 <- insertTestUser userWithMetadata2

        -- Insert staged upload for user1
        let stagedUploadInsert = completeInsert userId1 token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Try to claim as user2 (should fail)
        mResult <- TRX.statement () (UUT.claimUpload token userId2)
        TRX.condemn
        pure mResult

      assert $ do
        mClaimed <- assertRight result
        -- Should return Nothing because wrong user
        assertNothing mClaimed

-- | claimUpload fails for already claimed upload
prop_claimUploadAlreadyClaimed :: TestDBConfig -> PropertyT IO ()
prop_claimUploadAlreadyClaimed cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user
        userId <- insertTestUser userWithMetadata

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Claim it once (should succeed)
        firstClaim <- TRX.statement () (UUT.claimUpload token userId)

        -- Try to claim again (should fail)
        secondClaim <- TRX.statement () (UUT.claimUpload token userId)
        TRX.condemn
        pure (firstClaim, secondClaim)

      assert $ do
        (mFirst, mSecond) <- assertRight result
        -- First claim should succeed
        _ <- assertJust mFirst
        -- Second claim should fail (already claimed)
        assertNothing mSecond

-- | deleteById removes the upload
prop_deleteById :: TestDBConfig -> PropertyT IO ()
prop_deleteById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user
        userId <- insertTestUser userWithMetadata

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        mUploadId <- TRX.statement () (UUT.insert stagedUploadInsert)

        deleteResult <- case mUploadId of
          Nothing -> pure (Nothing, Nothing, Nothing)
          Just uploadId -> do
            -- Delete by ID
            deletedId <- TRX.statement () (UUT.deleteById uploadId)
            -- Verify it's gone
            afterDelete <- TRX.statement () (UUT.getByToken token)
            pure (Just uploadId, deletedId, afterDelete)
        TRX.condemn
        pure deleteResult

      assert $ do
        (mUploadId, mDeletedId, mAfterDelete) <- assertRight result
        uploadId <- assertJust mUploadId
        deletedId <- assertJust mDeletedId
        -- Delete should return the same ID
        deletedId === uploadId
        -- After delete, getByToken should return Nothing
        assertNothing mAfterDelete

-- | getExpiredUploads returns only expired pending uploads
prop_getExpiredUploads :: TestDBConfig -> PropertyT IO ()
prop_getExpiredUploads cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user
        userId <- insertTestUser userWithMetadata

        -- Insert a staged upload normally (not expired)
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Force the upload to be expired by setting expires_at to the past
        TRX.statement () $
          interp @()
            False
            [sql|
          UPDATE staged_uploads
          SET expires_at = NOW() - INTERVAL '1 hour'
          WHERE token = #{token}
        |]

        -- Get expired uploads
        expired <- TRX.statement () UUT.getExpiredUploads

        TRX.condemn
        pure (token, expired)

      assert $ do
        (insertedToken, expired) <- assertRight result
        -- Should contain our expired upload
        let tokens = map UUT.token expired
        elem insertedToken tokens === True

-- | getExpiredUploads returns empty list when no uploads are expired.
prop_getExpiredUploads_empty :: TestDBConfig -> PropertyT IO ()
prop_getExpiredUploads_empty cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    partialUpload <- forAllT partialStagedUploadInsertGen
    act $ do
      token <- liftIO generateSecureToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Do NOT expire the upload — it should remain non-expired
        expired <- TRX.statement () UUT.getExpiredUploads
        TRX.condemn
        pure expired

      assert $ do
        expired <- assertRight result
        length expired === 0
