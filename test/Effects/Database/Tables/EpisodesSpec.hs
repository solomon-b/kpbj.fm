module Effects.Database.Tables.EpisodesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as UUT
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
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.Episodes (episodeInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Episodes" $ do
      runs 10 . it "lens law: insert-select episode" $ hedgehog . prop_insertSelectEpisode
      runs 10 . it "lens law: getEpisodeById returns inserted episode" $ hedgehog . prop_getEpisodeById

-- Commenting out as there is no getEpisodesByShow function
-- runs 10 . it "query: getEpisodesByShow returns episodes for specific show" $ hedgehog . prop_getEpisodesByShow

-- Lens Law: insert then select returns what we inserted
prop_insertSelectEpisode :: TestDBConfig -> PropertyT IO ()
prop_insertSelectEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (User.Id 1) -- Placeholder IDs
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Setup: Create user, metadata, and show
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)
        showId <- TRX.statement () (Shows.insertShow showInsert)

        -- Update episode template with real IDs
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiCreatedBy = userId}

        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)
        selected <- TRX.statement () (UUT.getEpisodeById episodeId)
        pure (episodeId, episodeInsert, selected)

      assert $ do
        (episodeId, episodeInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.eiId episodeInsert === UUT.showId selected
        UUT.eiDescription episodeInsert === UUT.description selected
        UUT.eiStatus episodeInsert === UUT.status selected
        episodeId === UUT.id selected

-- Lens Law: getById after insert returns the episode
prop_getEpisodeById :: TestDBConfig -> PropertyT IO ()
prop_getEpisodeById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiCreatedBy = userId}

        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)
        byId <- TRX.statement () (UUT.getEpisodeById episodeId)
        pure (episodeId, byId)

      assert $ do
        (episodeId, mById) <- assertRight result
        byId <- assertJust mById
        UUT.id byId === episodeId
        pure ()

-- Commented out as getEpisodesByShow doesn't exist
-- We would need to implement it if needed for testing
