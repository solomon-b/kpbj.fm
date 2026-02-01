module Effects.Database.Tables.StagedUploadsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StagedUploads qualified as UUT
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.StagedUploads (generateSecureToken)
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
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
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Complete the insert with user ID and token
        let stagedUploadInsert = completeInsert userId token partialUpload
        mUploadId <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Fetch by token
        selected <- TRX.statement () (UUT.getByToken token)
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
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Claim it
        claimed <- TRX.statement () (UUT.claimUpload token userId)

        -- Verify status changed
        afterClaim <- TRX.statement () (UUT.getByToken token)
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
        (OneRow userId1) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata1) (UserMetadata.uwmiPassword userWithMetadata1)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId1 (UserMetadata.uwmiDisplayName userWithMetadata1) (UserMetadata.uwmiFullName userWithMetadata1) (UserMetadata.uwmiAvatarUrl userWithMetadata1) (UserMetadata.uwmiUserRole userWithMetadata1) (UserMetadata.uwmiColorScheme userWithMetadata1) (UserMetadata.uwmiTheme userWithMetadata1)

        (OneRow userId2) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata2) (UserMetadata.uwmiPassword userWithMetadata2)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId2 (UserMetadata.uwmiDisplayName userWithMetadata2) (UserMetadata.uwmiFullName userWithMetadata2) (UserMetadata.uwmiAvatarUrl userWithMetadata2) (UserMetadata.uwmiUserRole userWithMetadata2) (UserMetadata.uwmiColorScheme userWithMetadata2) (UserMetadata.uwmiTheme userWithMetadata2)

        -- Insert staged upload for user1
        let stagedUploadInsert = completeInsert userId1 token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Try to claim as user2 (should fail)
        TRX.statement () (UUT.claimUpload token userId2)

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
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        _ <- TRX.statement () (UUT.insert stagedUploadInsert)

        -- Claim it once (should succeed)
        firstClaim <- TRX.statement () (UUT.claimUpload token userId)

        -- Try to claim again (should fail)
        secondClaim <- TRX.statement () (UUT.claimUpload token userId)
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
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        -- Insert staged upload
        let stagedUploadInsert = completeInsert userId token partialUpload
        mUploadId <- TRX.statement () (UUT.insert stagedUploadInsert)

        case mUploadId of
          Nothing -> pure (Nothing, Nothing, Nothing)
          Just uploadId -> do
            -- Delete by ID
            deletedId <- TRX.statement () (UUT.deleteById uploadId)
            -- Verify it's gone
            afterDelete <- TRX.statement () (UUT.getByToken token)
            pure (Just uploadId, deletedId, afterDelete)

      assert $ do
        (mUploadId, mDeletedId, mAfterDelete) <- assertRight result
        uploadId <- assertJust mUploadId
        deletedId <- assertJust mDeletedId
        -- Delete should return the same ID
        deletedId === uploadId
        -- After delete, getByToken should return Nothing
        assertNothing mAfterDelete
