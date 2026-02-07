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
import Test.Database.Helpers (insertTestUser)
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
        mInsertedId <- TRX.statement () (UUT.insertEphemeralUpload uploadInsert)

        mSelected <- case mInsertedId of
          Nothing -> pure Nothing
          Just insertedId -> TRX.statement () (UUT.getEphemeralUploadById insertedId)

        TRX.condemn
        pure (mInsertedId, uploadInsert, mSelected)

      assert $ do
        (mInsertedId, uploadInsert, mSelected) <- assertRight result
        insertedId <- assertJust mInsertedId
        selected <- assertJust mSelected
        UUT.eumId selected === insertedId
        UUT.eumTitle selected === UUT.euiTitle uploadInsert
        UUT.eumAudioFilePath selected === UUT.euiAudioFilePath uploadInsert
        UUT.eumMimeType selected === UUT.euiMimeType uploadInsert
        UUT.eumFileSize selected === UUT.euiFileSize uploadInsert
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

        _ <- TRX.statement () (UUT.insertEphemeralUpload eu1)
        _ <- TRX.statement () (UUT.insertEphemeralUpload eu2)
        _ <- TRX.statement () (UUT.insertEphemeralUpload eu3)

        allItems <- TRX.statement () (UUT.getAllEphemeralUploads (Limit 10) (Offset 0))
        limited <- TRX.statement () (UUT.getAllEphemeralUploads (Limit 2) (Offset 0))
        offset <- TRX.statement () (UUT.getAllEphemeralUploads (Limit 10) (Offset 2))

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
        _ <- TRX.statement () (UUT.insertEphemeralUpload uploadInsert)

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
        mInsertedId <- TRX.statement () (UUT.insertEphemeralUpload uploadInsert)

        mUpdated <- case mInsertedId of
          Nothing -> pure Nothing
          Just insertedId -> do
            let newTitle = "Updated Title"
            let newPath = "/updated/path.mp3"
            let newMime = "audio/wav"
            let newSize = 42000
            TRX.statement () (UUT.updateEphemeralUpload insertedId newTitle newPath newMime newSize)

        TRX.condemn
        pure (mInsertedId, mUpdated)

      assert $ do
        (mInsertedId, mUpdated) <- assertRight result
        insertedId <- assertJust mInsertedId
        updated <- assertJust mUpdated
        UUT.eumId updated === insertedId
        UUT.eumTitle updated === "Updated Title"
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
        mInsertedId <- TRX.statement () (UUT.insertEphemeralUpload uploadInsert)

        (deleteResult, afterDelete) <- case mInsertedId of
          Nothing -> pure (Nothing, Nothing)
          Just insertedId -> do
            dr <- TRX.statement () (UUT.deleteEphemeralUpload insertedId)
            ad <- TRX.statement () (UUT.getEphemeralUploadById insertedId)
            pure (dr, ad)

        TRX.condemn
        pure (mInsertedId, deleteResult, afterDelete)

      assert $ do
        (mInsertedId, deleteResult, afterDelete) <- assertRight result
        insertedId <- assertJust mInsertedId
        deletedId <- assertJust deleteResult
        deletedId === insertedId
        assertNothing afterDelete
        pure ()
