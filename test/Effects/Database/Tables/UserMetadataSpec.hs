module Effects.Database.Tables.UserMetadataSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UUT
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.EmailAddress ()
import Test.Gen.Password ()
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.UserMetadata" $ do
      runs 10 . it "schema validation: insert user with metadata" $ hedgehog . prop_insertUserWithMetadata

-- Simplified test that works end-to-end
prop_insertUserWithMetadata :: TestDBConfig -> PropertyT IO ()
prop_insertUserWithMetadata cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UUT.uwmiEmail userWithMetadata) (UUT.uwmiPassword userWithMetadata)
        (OneRow metadataId) <- TRX.statement () $ UUT.insertUserMetadata $ UUT.ModelInsert userId (UUT.uwmiDisplayName userWithMetadata) (UUT.uwmiFullName userWithMetadata) (UUT.uwmiAvatarUrl userWithMetadata) (UUT.uwmiUserRole userWithMetadata)
        user <- TRX.statement () (User.getUser userId)
        metadata <- TRX.statement () (UUT.getUserMetadata userId)
        pure (userId, metadataId, user, metadata)

      assert $ do
        (userId, _metadataId, mUser, mMetadata) <- assertRight result
        user <- assertJust mUser
        metadata <- assertJust mMetadata
        User.mEmail user === UUT.uwmiEmail userWithMetadata
        UUT.mUserId metadata === userId
        UUT.mDisplayName metadata === UUT.uwmiDisplayName userWithMetadata
        UUT.mFullName metadata === UUT.uwmiFullName userWithMetadata
        UUT.mAvatarUrl metadata === UUT.uwmiAvatarUrl userWithMetadata
        UUT.mUserRole metadata === UUT.uwmiUserRole userWithMetadata
