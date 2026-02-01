module Effects.Database.Tables.EventsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as UUT
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
import Test.Gen.Tables.Events (eventInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Events" $ do
      runs 10 . it "lens law: insert-select event" $ hedgehog . prop_insertSelectEvent
      runs 10 . it "lens law: getEventById returns inserted event" $ hedgehog . prop_getEventById

-- Lens Law: insert then select returns what we inserted
prop_insertSelectEvent :: TestDBConfig -> PropertyT IO ()
prop_insertSelectEvent cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        let eventInsert = eventTemplate {UUT.eiAuthorId = userId}

        eventId <- TRX.statement () (UUT.insertEvent eventInsert)
        selected <- TRX.statement () (UUT.getEventById eventId)
        pure (eventId, eventInsert, selected)

      assert $ do
        (eventId, eventInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.eiTitle eventInsert === UUT.emTitle selected
        UUT.eiSlug eventInsert === UUT.emSlug selected
        UUT.eiDescription eventInsert === UUT.emDescription selected
        UUT.eiLocationName eventInsert === UUT.emLocationName selected
        UUT.eiLocationAddress eventInsert === UUT.emLocationAddress selected
        UUT.eiStatus eventInsert === UUT.emStatus selected
        UUT.eiAuthorId eventInsert === UUT.emAuthorId selected
        eventId === UUT.emId selected
        pure ()

-- Lens Law: getById after insert returns the event
prop_getEventById :: TestDBConfig -> PropertyT IO ()
prop_getEventById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    eventTemplate <- forAllT $ eventInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        let eventInsert = eventTemplate {UUT.eiAuthorId = userId}

        eventId <- TRX.statement () (UUT.insertEvent eventInsert)
        byId <- TRX.statement () (UUT.getEventById eventId)
        pure (eventId, byId)

      assert $ do
        (eventId, mById) <- assertRight result
        byId <- assertJust mById
        UUT.emId byId === eventId
        pure ()
