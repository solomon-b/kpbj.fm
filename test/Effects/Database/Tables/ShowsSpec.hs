module Effects.Database.Tables.ShowsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Shows" $ do
      runs 30 . it "schema validation: insert and select show" $ hedgehog . prop_insertSelect
      runs 30 . it "query validation: getShowBySlug" $ hedgehog . prop_getShowBySlug

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertedId <- TRX.statement () (UUT.insertShow showInsert)
        selected <- TRX.statement () (UUT.getShowById insertedId)
        pure (insertedId, selected)

      assert $ do
        (insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.siTitle showInsert === UUT.title selected
        UUT.siSlug showInsert === UUT.slug selected
        UUT.siDescription showInsert === UUT.description selected
        UUT.siGenre showInsert === UUT.genre selected
        UUT.siLogoUrl showInsert === UUT.logoUrl selected
        UUT.siBannerUrl showInsert === UUT.bannerUrl selected
        UUT.siStatus showInsert === UUT.status selected
        UUT.siFrequency showInsert === UUT.frequency selected
        UUT.siDurationMinutes showInsert === UUT.durationMinutes selected
        insertedId === UUT.id selected

prop_getShowBySlug :: TestDBConfig -> PropertyT IO ()
prop_getShowBySlug cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        insertedId <- TRX.statement () (UUT.insertShow showInsert)
        byId <- TRX.statement () (UUT.getShowById insertedId)
        bySlug <- TRX.statement () (UUT.getShowBySlug $ UUT.siSlug showInsert)
        pure (insertedId, byId, bySlug)

      assert $ do
        (insertedId, mById, mBySlug) <- assertRight result
        byId <- assertJust mById
        bySlug <- assertJust mBySlug
        UUT.id byId === UUT.id bySlug
        UUT.slug byId === UUT.slug bySlug
        insertedId === UUT.id bySlug
