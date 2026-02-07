{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.PasswordResetTokensSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.PasswordResetTokens qualified as UUT
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.PasswordReset (generateResetToken)
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight)
import Test.Gen.Tables.PasswordResetTokens (mkInsert, passwordResetInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.PasswordResetTokens" $ do
      -- Lens Laws
      runs 10 . it "lens law: insert and getByToken returns correct model" $ hedgehog . prop_insertAndGetByToken

      -- Token Consumption
      runs 10 . it "consume: consumeToken returns model with Used status" $ hedgehog . prop_consumeToken
      runs 5 . it "consume: expired token returns Nothing" $ hedgehog . prop_consumeToken_expired
      runs 5 . it "consume: already used token returns Nothing" $ hedgehog . prop_consumeToken_alreadyUsed
      runs 5 . it "consume: getByToken returns Nothing after consumption" $ hedgehog . prop_getByToken_afterConsume

      -- Invalidation
      runs 5 . it "invalidate: expirePendingForUser expires all pending tokens" $ hedgehog . prop_expirePendingForUser

      -- Rate Limiting
      runs 5 . it "rate limit: countRecentForEmail returns correct count" $ hedgehog . prop_countRecentForEmail

      -- Cleanup
      runs 5 . it "cleanup: deleteExpired removes expired pending tokens" $ hedgehog . prop_deleteExpired
      runs 5 . it "cleanup: deleteOlderThanDays removes old tokens" $ hedgehog . prop_deleteOlderThanDays

      -- Audit Fields
      runs 10 . it "audit: ip_address and user_agent preserved" $ hedgehog . prop_auditFieldsPreserved

--------------------------------------------------------------------------------
-- Property Tests

-- | Insert a token, getByToken returns model with correct fields.
prop_insertAndGetByToken :: TestDBConfig -> PropertyT IO ()
prop_insertAndGetByToken cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    (ipAddress, userAgent) <- forAllT passwordResetInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText ipAddress userAgent
        mId <- TRX.statement () (UUT.insert ins)
        mModel <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure (mId, userId, emailText, token, ipAddress, userAgent, mModel)

      assert $ do
        (mId, userId, emailText, tk, ip, ua, mModel) <- assertRight result
        _ <- assertJust mId
        model <- assertJust mModel
        UUT.userId model === userId
        UUT.token model === tk
        UUT.email model === emailText
        UUT.status model === UUT.Pending
        UUT.ipAddress model === ip
        UUT.userAgent model === ua
        assertNothing (UUT.usedAt model)

-- | consumeToken returns model with Used status and usedAt set.
prop_consumeToken :: TestDBConfig -> PropertyT IO ()
prop_consumeToken cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText Nothing Nothing
        _ <- TRX.statement () (UUT.insert ins)
        mResult <- TRX.statement () (UUT.consumeToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        model <- assertJust mModel
        UUT.status model === UUT.Used
        _ <- assertJust (UUT.usedAt model)
        pure ()

-- | Force expires_at to past, consumeToken returns Nothing.
prop_consumeToken_expired :: TestDBConfig -> PropertyT IO ()
prop_consumeToken_expired cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText Nothing Nothing
        _ <- TRX.statement () (UUT.insert ins)

        -- Force token to be expired
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE password_reset_tokens
              SET expires_at = NOW() - INTERVAL '1 hour'
              WHERE token = #{token}
            |]

        mResult <- TRX.statement () (UUT.consumeToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Second consumeToken returns Nothing (single-use).
prop_consumeToken_alreadyUsed :: TestDBConfig -> PropertyT IO ()
prop_consumeToken_alreadyUsed cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText Nothing Nothing
        _ <- TRX.statement () (UUT.insert ins)
        firstConsume <- TRX.statement () (UUT.consumeToken token)
        secondConsume <- TRX.statement () (UUT.consumeToken token)
        TRX.condemn
        pure (firstConsume, secondConsume)

      assert $ do
        (first', second') <- assertRight result
        _ <- assertJust first'
        assertNothing second'

-- | getByToken returns Nothing after token is consumed.
prop_getByToken_afterConsume :: TestDBConfig -> PropertyT IO ()
prop_getByToken_afterConsume cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText Nothing Nothing
        _ <- TRX.statement () (UUT.insert ins)
        _ <- TRX.statement () (UUT.consumeToken token)
        mResult <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Insert two tokens, expirePendingForUser, both unresolvable via getByToken.
prop_expirePendingForUser :: TestDBConfig -> PropertyT IO ()
prop_expirePendingForUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token1 <- liftIO generateResetToken
      token2 <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- TRX.statement () (UUT.insert (mkInsert userId token1 emailText Nothing Nothing))
        _ <- TRX.statement () (UUT.insert (mkInsert userId token2 emailText Nothing Nothing))
        TRX.statement () (UUT.expirePendingForUser userId)
        g1 <- TRX.statement () (UUT.getByToken token1)
        g2 <- TRX.statement () (UUT.getByToken token2)
        TRX.condemn
        pure (g1, g2)

      assert $ do
        (g1, g2) <- assertRight result
        assertNothing g1
        assertNothing g2

-- | Insert N tokens for same email, countRecentForEmail returns N.
prop_countRecentForEmail :: TestDBConfig -> PropertyT IO ()
prop_countRecentForEmail cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token1 <- liftIO generateResetToken
      token2 <- liftIO generateResetToken
      token3 <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- TRX.statement () (UUT.insert (mkInsert userId token1 emailText Nothing Nothing))
        _ <- TRX.statement () (UUT.insert (mkInsert userId token2 emailText Nothing Nothing))
        _ <- TRX.statement () (UUT.insert (mkInsert userId token3 emailText Nothing Nothing))
        mResult <- TRX.statement () (UUT.countRecentForEmail emailText)
        TRX.condemn
        pure mResult

      assert $ do
        count <- assertRight result
        count === 3

-- | Force token to expired, deleteExpired removes it.
prop_deleteExpired :: TestDBConfig -> PropertyT IO ()
prop_deleteExpired cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- TRX.statement () (UUT.insert (mkInsert userId token emailText Nothing Nothing))

        -- Force token to be expired
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE password_reset_tokens
              SET expires_at = NOW() - INTERVAL '1 hour'
              WHERE token = #{token}
            |]

        TRX.statement () UUT.deleteExpired
        mResult <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Force created_at to 31 days ago, deleteOlderThanDays 30 removes it.
prop_deleteOlderThanDays :: TestDBConfig -> PropertyT IO ()
prop_deleteOlderThanDays cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- TRX.statement () (UUT.insert (mkInsert userId token emailText Nothing Nothing))

        -- Force created_at to 31 days ago
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE password_reset_tokens
              SET created_at = NOW() - INTERVAL '31 days'
              WHERE token = #{token}
            |]

        TRX.statement () (UUT.deleteOlderThanDays 30)
        mResult <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Insert with ip_address and user_agent, getByToken returns them intact.
prop_auditFieldsPreserved :: TestDBConfig -> PropertyT IO ()
prop_auditFieldsPreserved cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    (ipAddress, userAgent) <- forAllT passwordResetInsertGen
    act $ do
      token <- liftIO generateResetToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText ipAddress userAgent
        _ <- TRX.statement () (UUT.insert ins)
        mResult <- TRX.statement () (UUT.getByToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        model <- assertJust mModel
        UUT.ipAddress model === ipAddress
        UUT.userAgent model === userAgent
