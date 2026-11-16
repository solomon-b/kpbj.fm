{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.HostInvitationSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Domain.Types.EmailAddress (EmailAddress, mkEmailAddress)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.HostInvitation qualified as UUT
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneColumn (..), OneRow (..), interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.HostInvitation" $ do
      runs 3 . it "insert and getById round-trip starts_on" $ hedgehog . prop_startsOnRoundTrip
      runs 3 . it "getById reads a pending row past expires_at as Expired" $ hedgehog . prop_getByIdExpired
      runs 3 . it "getAllWithCreator reads a pending row past expires_at as Expired" $ hedgehog . prop_getAllWithCreatorExpired
      runs 3 . it "getLive excludes expired, revoked, and claimed rows" $ hedgehog . prop_getLiveExcludesDeadRows
      runs 3 . it "updatePending writes the recipient, schedule, and start date" $ hedgehog . prop_updatePendingRoundTrip
      runs 3 . it "updatePending skips expired, revoked, and claimed rows" $ hedgehog . prop_updatePendingSkipsDeadRows

--------------------------------------------------------------------------------
-- Helpers

-- | The start date every test invitation uses.
testStartsOn :: Day
testStartsOn = fromGregorian 2027 1 15

-- | Move an invitation's expiry one minute into the past. The status stays pending.
expireInvitation :: UUT.Id -> TRX.Transaction ()
expireInvitation invId =
  TRX.statement () $
    interp
      False
      [sql| UPDATE host_invitations SET expires_at = NOW() - INTERVAL '1 minute' WHERE id = #{invId} |]

-- | The status as it is stored, with no computation on read.
storedStatus :: UUT.Id -> TRX.Transaction Text
storedStatus invId = do
  OneRow (OneColumn status) <-
    TRX.statement () $
      interp False [sql| SELECT status::text FROM host_invitations WHERE id = #{invId} |]
  pure status

--------------------------------------------------------------------------------
-- Property Tests

prop_startsOnRoundTrip :: TestDBConfig -> PropertyT IO ()
prop_startsOnRoundTrip cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO UUT.generateInviteCode
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        invId <- unwrapInsert (UUT.insert (mkInsert token userId))
        mModel <- TRX.statement () (UUT.getById invId)
        TRX.condemn
        pure mModel
      assert $ do
        mModel <- assertRight result
        model <- assertJust mModel
        model.hiStartsOn === testStartsOn

prop_getByIdExpired :: TestDBConfig -> PropertyT IO ()
prop_getByIdExpired cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO UUT.generateInviteCode
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        invId <- unwrapInsert (UUT.insert (mkInsert token userId))
        expireInvitation invId
        mModel <- TRX.statement () (UUT.getById invId)
        stored <- storedStatus invId
        TRX.condemn
        pure (mModel, stored)
      assert $ do
        (mModel, stored) <- assertRight result
        model <- assertJust mModel
        model.hiStatus === UUT.Expired
        stored === "pending"

prop_getAllWithCreatorExpired :: TestDBConfig -> PropertyT IO ()
prop_getAllWithCreatorExpired cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO UUT.generateInviteCode
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        invId <- unwrapInsert (UUT.insert (mkInsert token userId))
        expireInvitation invId
        rows <- TRX.statement () UUT.getAllWithCreator
        TRX.condemn
        pure (invId, rows)
      assert $ do
        (invId, rows) <- assertRight result
        row <- assertJust (List.find (\r -> r.mwcId == invId) rows)
        row.mwcStatus === UUT.Expired
        row.mwcStartsOn === testStartsOn

prop_getLiveExcludesDeadRows :: TestDBConfig -> PropertyT IO ()
prop_getLiveExcludesDeadRows cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      tokens <- liftIO $ traverse (const UUT.generateInviteCode) [1 :: Int .. 4]
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        ids <- traverse (\t -> unwrapInsert (UUT.insert (mkInsert t userId))) tokens
        case (ids, tokens) of
          ([liveId, expiredId, revokedId, claimedId], [_, _, _, claimedToken]) -> do
            expireInvitation expiredId
            _ <- TRX.statement () (UUT.revokeInvitation revokedId)
            _ <- TRX.statement () (UUT.claimInvitation claimedToken userId)
            live <- TRX.statement () UUT.getLive
            TRX.condemn
            pure (map (.hiId) live, liveId, [expiredId, revokedId, claimedId])
          _ -> error "expected four invitations"
      assert $ do
        (liveIds, liveId, deadIds) <- assertRight result
        Hedgehog.assert (liveId `elem` liveIds)
        filter (`elem` liveIds) deadIds === []

prop_updatePendingRoundTrip :: TestDBConfig -> PropertyT IO ()
prop_updatePendingRoundTrip cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO UUT.generateInviteCode
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        invId <- unwrapInsert (UUT.insert (mkInsert token userId))
        mUpdated <- TRX.statement () (UUT.updatePending invId newEmail newSchedule newStartsOn)
        mModel <- TRX.statement () (UUT.getById invId)
        TRX.condemn
        pure (mUpdated, mModel)
      assert $ do
        (mUpdated, mModel) <- assertRight result
        updated <- assertJust mUpdated
        model <- assertJust mModel
        updated === model
        model.hiRecipientEmail === newEmail
        model.hiScheduleData === newSchedule
        model.hiStartsOn === newStartsOn
        model.hiToken === token
        model.hiStatus === UUT.Pending

prop_updatePendingSkipsDeadRows :: TestDBConfig -> PropertyT IO ()
prop_updatePendingSkipsDeadRows cfg =
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      tokens <- liftIO $ traverse (const UUT.generateInviteCode) [1 :: Int .. 3]
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        ids <- traverse (\t -> unwrapInsert (UUT.insert (mkInsert t userId))) tokens
        case (ids, tokens) of
          ([expiredId, revokedId, _claimedId], [_, _, claimedToken]) -> do
            expireInvitation expiredId
            _ <- TRX.statement () (UUT.revokeInvitation revokedId)
            _ <- TRX.statement () (UUT.claimInvitation claimedToken userId)
            updates <- traverse (\invId -> TRX.statement () (UUT.updatePending invId newEmail newSchedule newStartsOn)) ids
            models <- traverse (TRX.statement () . UUT.getById) ids
            TRX.condemn
            pure (updates, models)
          _ -> error "expected three invitations"
      assert $ do
        (updates, models) <- assertRight result
        updates === [Nothing, Nothing, Nothing]
        map (fmap (.hiStartsOn)) models === replicate 3 (Just testStartsOn)

--------------------------------------------------------------------------------

-- | The recipient that the update tests write.
newEmail :: EmailAddress
newEmail = mkEmailAddress "moved@example.com"

-- | The schedule that the update tests write.
newSchedule :: Aeson.Value
newSchedule =
  Aeson.toJSON
    [ Aeson.object
        [ "dayOfWeek" Aeson..= ("tuesday" :: Text),
          "weeksOfMonth" Aeson..= ([1, 3] :: [Int]),
          "startTime" Aeson..= ("20:00" :: Text),
          "duration" Aeson..= (60 :: Int)
        ]
    ]

-- | The start date that the update tests write.
newStartsOn :: Day
newStartsOn = fromGregorian 2027 2 1

-- | An invitation insert with a fixed recipient, schedule, and start date.
mkInsert :: UUT.Token -> User.Id -> UUT.Insert
mkInsert token userId =
  UUT.Insert
    { UUT.iToken = token,
      UUT.iScheduleData = Aeson.toJSON ([] :: [Aeson.Value]),
      UUT.iRecipientEmail = mkEmailAddress "invitee@example.com",
      UUT.iCreatedBy = userId,
      UUT.iStartsOn = testStartsOn
    }
