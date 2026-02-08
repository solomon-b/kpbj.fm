module Effects.Database.Tables.StationIdsSpec where

--------------------------------------------------------------------------------

import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StationIds qualified as UUT
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (->-))
import Test.Gen.Tables.StationIds (stationIdInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.StationIds" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        runs 10 . it "getAllStationIds: returns paginated results" $
          hedgehog . prop_getAllStationIds_paginated

      describe "Mutations" $ do
        runs 10 . it "deleteStationId: removes station ID" $ hedgehog . prop_deleteStationId
        runs 10 . it "deleteStationId: returns Nothing for non-existent" $ hedgehog . prop_deleteStationId_notFound

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select by ID returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ stationIdInsertGen (User.Id 1) -- placeholder, overridden
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let stationIdInsert = template {UUT.siiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertStationId stationIdInsert)

        mSelected <- TRX.statement () (UUT.getStationIdById insertedId)

        TRX.condemn
        pure (insertedId, stationIdInsert, mSelected)

      assert $ do
        (insertedId, stationIdInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.simId selected === insertedId
        UUT.simTitle selected === UUT.siiTitle stationIdInsert
        UUT.simAudioFilePath selected === UUT.siiAudioFilePath stationIdInsert
        UUT.simMimeType selected === UUT.siiMimeType stationIdInsert
        UUT.simFileSize selected === UUT.siiFileSize stationIdInsert
        insertedId ->- UUT.Id 0
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getAllStationIds: returns paginated results with creator display name.
prop_getAllStationIds_paginated :: TestDBConfig -> PropertyT IO ()
prop_getAllStationIds_paginated cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ stationIdInsertGen (User.Id 1)
    template2 <- forAllT $ stationIdInsertGen (User.Id 1)
    template3 <- forAllT $ stationIdInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let si1 = template1 {UUT.siiCreatorId = userId}
        let si2 = template2 {UUT.siiCreatorId = userId}
        let si3 = template3 {UUT.siiCreatorId = userId}

        _ <- unwrapInsert (UUT.insertStationId si1)
        _ <- unwrapInsert (UUT.insertStationId si2)
        _ <- unwrapInsert (UUT.insertStationId si3)

        allItems <- TRX.statement () (UUT.getAllStationIds (Limit 10) (Offset 0))
        limited <- TRX.statement () (UUT.getAllStationIds (Limit 2) (Offset 0))
        offset <- TRX.statement () (UUT.getAllStationIds (Limit 10) (Offset 2))

        TRX.condemn
        pure (allItems, limited, offset)

      assert $ do
        (allItems, limited, offset) <- assertRight result
        length allItems === 3
        length limited === 2
        length offset === 1
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteStationId: removes station ID and returns its ID.
prop_deleteStationId :: TestDBConfig -> PropertyT IO ()
prop_deleteStationId cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ stationIdInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let stationIdInsert = template {UUT.siiCreatorId = userId}
        insertedId <- unwrapInsert (UUT.insertStationId stationIdInsert)

        deleteResult <- TRX.statement () (UUT.deleteStationId insertedId)
        afterDelete <- TRX.statement () (UUT.getStationIdById insertedId)

        TRX.condemn
        pure (insertedId, deleteResult, afterDelete)

      assert $ do
        (insertedId, deleteResult, afterDelete) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === insertedId
        assertNothing afterDelete
        pure ()

-- | deleteStationId: returns Nothing for non-existent ID.
prop_deleteStationId_notFound :: TestDBConfig -> PropertyT IO ()
prop_deleteStationId_notFound cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        deleteResult <- TRX.statement () (UUT.deleteStationId (UUT.Id 999999))
        TRX.condemn
        pure deleteResult

      assert $ do
        deleteResult <- assertRight result
        assertNothing deleteResult
        pure ()
