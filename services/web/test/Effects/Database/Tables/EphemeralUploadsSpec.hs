{-# LANGUAGE OverloadedRecordDot #-}

module Effects.Database.Tables.EphemeralUploadsSpec where

--------------------------------------------------------------------------------

import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EphemeralUploads qualified as UUT
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (->-))
import Test.Gen.Tables.EphemeralUploads (ephemeralUploadInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.EphemeralUploads" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        runs 10 . it "getAllEphemeralUploads: returns paginated results" $
          hedgehog . prop_getAllEphemeralUploads_paginated
        runs 10 . it "getRandomEphemeralUpload: returns a valid upload when entries exist" $
          hedgehog . prop_getRandomEphemeralUpload

      describe "Mutations" $ do
        runs 10 . it "updateEphemeralUpload: updates fields" $ hedgehog . prop_updateEphemeralUpload
        runs 10 . it "deleteEphemeralUpload: removes upload" $ hedgehog . prop_deleteEphemeralUpload

      describe "Flagging" $ do
        runs 10 . it "flagEphemeralUpload: sets flagged fields" $ hedgehog . prop_flagEphemeralUpload
        runs 10 . it "unflagEphemeralUpload: clears flagged fields" $ hedgehog . prop_unflagEphemeralUpload
        runs 10 . it "flagged uploads excluded from getRandomEphemeralUpload" $ hedgehog . prop_flaggedExcludedFromRandom
        runs 10 . it "getAllEphemeralUploads excludes flagged when includeFlagged=False" $ hedgehog . prop_getAllExcludesFlagged
        runs 10 . it "getAllEphemeralUploads includes flagged when includeFlagged=True" $ hedgehog . prop_getAllIncludesFlagged

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select by ID returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        mSelected <- TRX.statement () (UUT.getEphemeralUploadById insertedId)

        TRX.condemn
        pure (insertedId, uploadInsert, mSelected)

      assert $ do
        (insertedId, uploadInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.eumId selected === insertedId
        UUT.eumTitle selected === UUT.euiTitle uploadInsert
        UUT.eumDescription selected === UUT.euiDescription uploadInsert
        UUT.eumAudioFilePath selected === UUT.euiAudioFilePath uploadInsert
        UUT.eumMimeType selected === UUT.euiMimeType uploadInsert
        UUT.eumFileSize selected === UUT.euiFileSize uploadInsert
        UUT.eumFlaggedAt selected === Nothing
        UUT.eumFlaggedBy selected === Nothing
        UUT.eumFlagReason selected === Nothing
        insertedId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getAllEphemeralUploads: returns paginated results with creator display name.
prop_getAllEphemeralUploads_paginated :: TestDBConfig -> PropertyT IO ()
prop_getAllEphemeralUploads_paginated cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)
    template2 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)
    template3 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let eu1 = template1 {UUT.euiCreatorId = userId}
        let eu2 = template2 {UUT.euiCreatorId = userId}
        let eu3 = template3 {UUT.euiCreatorId = userId}

        _ <- unwrapInsert (UUT.insertEphemeralUpload eu1)
        _ <- unwrapInsert (UUT.insertEphemeralUpload eu2)
        _ <- unwrapInsert (UUT.insertEphemeralUpload eu3)

        allItems <- TRX.statement () (UUT.getAllEphemeralUploads True (Limit 10) (Offset 0))
        limited <- TRX.statement () (UUT.getAllEphemeralUploads True (Limit 2) (Offset 0))
        offset <- TRX.statement () (UUT.getAllEphemeralUploads True (Limit 10) (Offset 2))

        TRX.condemn
        pure (allItems, limited, offset)

      assert $ do
        (allItems, limited, offset) <- assertRight result
        length allItems === 3
        length limited === 2
        length offset === 1
        pure ()

-- | getRandomEphemeralUpload: returns a valid upload when entries exist.
prop_getRandomEphemeralUpload :: TestDBConfig -> PropertyT IO ()
prop_getRandomEphemeralUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        _ <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        randomUpload <- TRX.statement () UUT.getRandomEphemeralUpload
        TRX.condemn
        pure randomUpload

      assert $ do
        mRandom <- assertRight result
        -- Should return something since we inserted one
        _ <- assertJust mRandom
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | updateEphemeralUpload: updates fields and returns updated model.
prop_updateEphemeralUpload :: TestDBConfig -> PropertyT IO ()
prop_updateEphemeralUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        let newTitle = "Updated Title"
        let newDescription = "Updated description for testing purposes"
        let newPath = "/updated/path.mp3"
        let newMime = "audio/wav"
        let newSize = 42000
        mUpdated <- TRX.statement () (UUT.updateEphemeralUpload insertedId newTitle newDescription newPath newMime newSize)

        TRX.condemn
        pure (insertedId, mUpdated)

      assert $ do
        (insertedId, mUpdated) <- assertRight result
        updated <- assertJust mUpdated
        UUT.eumId updated === insertedId
        UUT.eumTitle updated === "Updated Title"
        UUT.eumDescription updated === "Updated description for testing purposes"
        UUT.eumAudioFilePath updated === "/updated/path.mp3"
        UUT.eumMimeType updated === "audio/wav"
        UUT.eumFileSize updated === 42000
        pure ()

-- | deleteEphemeralUpload: removes upload, returns its ID.
prop_deleteEphemeralUpload :: TestDBConfig -> PropertyT IO ()
prop_deleteEphemeralUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        deleteResult <- TRX.statement () (UUT.deleteEphemeralUpload insertedId)
        afterDelete <- TRX.statement () (UUT.getEphemeralUploadById insertedId)

        TRX.condemn
        pure (insertedId, deleteResult, afterDelete)

      assert $ do
        (insertedId, deleteResult, afterDelete) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === insertedId
        assertNothing afterDelete
        pure ()

--------------------------------------------------------------------------------
-- Flagging tests

-- | flagEphemeralUpload: sets flagged_at, flagged_by, and flag_reason.
prop_flagEphemeralUpload :: TestDBConfig -> PropertyT IO ()
prop_flagEphemeralUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        mFlagged <- TRX.statement () (UUT.flagEphemeralUpload insertedId userId UUT.InappropriateContent)
        mAfterFlag <- TRX.statement () (UUT.getEphemeralUploadById insertedId)

        TRX.condemn
        pure (insertedId, userId, mFlagged, mAfterFlag)

      assert $ do
        (insertedId, userId, mFlagged, mAfterFlag) <- assertRight result
        flagged <- assertJust mFlagged
        UUT.eumId flagged === insertedId
        UUT.eumFlaggedBy flagged === Just userId
        UUT.eumFlagReason flagged === Just UUT.InappropriateContent
        -- flagged_at should be set (not Nothing)
        afterFlag <- assertJust mAfterFlag
        afterFlag.eumFlaggedBy === Just userId
        afterFlag.eumFlagReason === Just UUT.InappropriateContent
        _ <- assertJust (UUT.eumFlaggedAt afterFlag)
        pure ()

-- | unflagEphemeralUpload: clears flagged fields.
prop_unflagEphemeralUpload :: TestDBConfig -> PropertyT IO ()
prop_unflagEphemeralUpload cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        -- Flag first
        _ <- TRX.statement () (UUT.flagEphemeralUpload insertedId userId UUT.PoorAudioQuality)
        -- Then unflag
        mUnflagged <- TRX.statement () (UUT.unflagEphemeralUpload insertedId)
        mAfterUnflag <- TRX.statement () (UUT.getEphemeralUploadById insertedId)

        TRX.condemn
        pure (insertedId, mUnflagged, mAfterUnflag)

      assert $ do
        (insertedId, mUnflagged, mAfterUnflag) <- assertRight result
        unflagged <- assertJust mUnflagged
        UUT.eumId unflagged === insertedId
        UUT.eumFlaggedAt unflagged === Nothing
        UUT.eumFlaggedBy unflagged === Nothing
        UUT.eumFlagReason unflagged === Nothing
        afterUnflag <- assertJust mAfterUnflag
        UUT.eumFlaggedAt afterUnflag === Nothing
        UUT.eumFlaggedBy afterUnflag === Nothing
        UUT.eumFlagReason afterUnflag === Nothing
        pure ()

-- | Flagged uploads are excluded from getRandomEphemeralUpload.
prop_flaggedExcludedFromRandom :: TestDBConfig -> PropertyT IO ()
prop_flaggedExcludedFromRandom cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let uploadInsert = template {UUT.euiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertEphemeralUpload uploadInsert)

        -- Flag the only upload
        _ <- TRX.statement () (UUT.flagEphemeralUpload insertedId userId UUT.CopyrightConcern)
        -- Random should return Nothing now
        randomUpload <- TRX.statement () UUT.getRandomEphemeralUpload

        TRX.condemn
        pure randomUpload

      assert $ do
        mRandom <- assertRight result
        assertNothing mRandom
        pure ()

-- | getAllEphemeralUploads with includeFlagged=False excludes flagged uploads.
prop_getAllExcludesFlagged :: TestDBConfig -> PropertyT IO ()
prop_getAllExcludesFlagged cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)
    template2 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let eu1 = template1 {UUT.euiCreatorId = userId}
        let eu2 = template2 {UUT.euiCreatorId = userId}

        id1 <- unwrapInsert (UUT.insertEphemeralUpload eu1)
        _ <- unwrapInsert (UUT.insertEphemeralUpload eu2)

        -- Flag the first upload
        _ <- TRX.statement () (UUT.flagEphemeralUpload id1 userId UUT.InappropriateContent)

        unflaggedOnly <- TRX.statement () (UUT.getAllEphemeralUploads False (Limit 10) (Offset 0))

        TRX.condemn
        pure unflaggedOnly

      assert $ do
        unflaggedOnly <- assertRight result
        length unflaggedOnly === 1
        pure ()

-- | getAllEphemeralUploads with includeFlagged=True returns all uploads.
prop_getAllIncludesFlagged :: TestDBConfig -> PropertyT IO ()
prop_getAllIncludesFlagged cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)
    template2 <- forAllT $ ephemeralUploadInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let eu1 = template1 {UUT.euiCreatorId = userId}
        let eu2 = template2 {UUT.euiCreatorId = userId}

        id1 <- unwrapInsert (UUT.insertEphemeralUpload eu1)
        _ <- unwrapInsert (UUT.insertEphemeralUpload eu2)

        -- Flag the first upload
        _ <- TRX.statement () (UUT.flagEphemeralUpload id1 userId UUT.PoorAudioQuality)

        allItems <- TRX.statement () (UUT.getAllEphemeralUploads True (Limit 10) (Offset 0))

        TRX.condemn
        pure allItems

      assert $ do
        allItems <- assertRight result
        length allItems === 2
        pure ()
