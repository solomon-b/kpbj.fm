{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.EmailVerificationTokensSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EmailVerificationTokens qualified as UUT
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.EmailVerification (generateVerificationToken)
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (<==))
import Test.Gen.Tables.EmailVerificationTokens (mkInsert)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.EmailVerificationTokens" $ do
      -- Lens Laws
      runs 10 . it "lens law: insert and verify returns correct model" $ hedgehog . prop_insertAndVerify

      -- Token Verification
      runs 10 . it "verify: sets user email_verified" $ hedgehog . prop_verifyToken_setsUserEmailVerified
      runs 5 . it "verify: expired token returns Nothing" $ hedgehog . prop_verifyToken_expired
      runs 5 . it "verify: already verified token returns Nothing" $ hedgehog . prop_verifyToken_alreadyVerified

      -- Invalidation
      runs 5 . it "invalidate: invalidateForUser expires all pending tokens" $ hedgehog . prop_invalidateForUser

      -- Rate Limiting
      runs 5 . it "rate limit: getLastTokenCreatedAt within cooldown returns Just" $ hedgehog . prop_getLastTokenCreatedAt_withinCooldown
      runs 5 . it "rate limit: getLastTokenCreatedAt outside cooldown returns Nothing" $ hedgehog . prop_getLastTokenCreatedAt_outsideCooldown

      -- Cleanup
      runs 5 . it "cleanup: deleteExpired removes expired pending tokens" $ hedgehog . prop_deleteExpired
      runs 5 . it "cleanup: deleteOlderThanDays removes old tokens" $ hedgehog . prop_deleteOlderThanDays

      -- Foreign Key Constraints
      runs 5 . it "constraint: insert with non-existent user fails" $ hedgehog . prop_insertNonExistentUser

      -- User Status
      runs 10 . it "status: isUserEmailVerified before verification returns Nothing" $ hedgehog . prop_isUserEmailVerified_beforeVerification

--------------------------------------------------------------------------------
-- Property Tests

-- | Insert a token, verify it, check the returned model has correct fields.
prop_insertAndVerify :: TestDBConfig -> PropertyT IO ()
prop_insertAndVerify cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText
        _ <- unwrapInsert (UUT.insert ins)
        mModel <- TRX.statement () (UUT.verifyToken token)
        TRX.condemn
        pure (userId, emailText, token, mModel)

      assert $ do
        (userId, emailText, tk, mModel) <- assertRight result
        model <- assertJust mModel
        UUT.evtUserId model === userId
        UUT.evtToken model === tk
        UUT.evtEmail model === emailText
        UUT.evtStatus model === UUT.Verified
        _ <- assertJust (UUT.evtVerifiedAt model)
        pure ()

-- | After verifyToken, isUserEmailVerified returns Just userId.
prop_verifyToken_setsUserEmailVerified :: TestDBConfig -> PropertyT IO ()
prop_verifyToken_setsUserEmailVerified cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText
        _ <- unwrapInsert (UUT.insert ins)
        _ <- TRX.statement () (UUT.verifyToken token)
        mVerified <- TRX.statement () (UUT.isUserEmailVerified userId)
        TRX.condemn
        pure (userId, mVerified)

      assert $ do
        (userId, mVerified) <- assertRight result
        verifiedUserId <- assertJust mVerified
        verifiedUserId === userId

-- | Force expires_at to past, verifyToken returns Nothing.
prop_verifyToken_expired :: TestDBConfig -> PropertyT IO ()
prop_verifyToken_expired cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText
        _ <- unwrapInsert (UUT.insert ins)

        -- Force token to be expired
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE email_verification_tokens
              SET expires_at = NOW() - INTERVAL '1 hour'
              WHERE token = #{token}
            |]

        mResult <- TRX.statement () (UUT.verifyToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Second verifyToken call returns Nothing (single-use).
prop_verifyToken_alreadyVerified :: TestDBConfig -> PropertyT IO ()
prop_verifyToken_alreadyVerified cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
            ins = mkInsert userId token emailText
        _ <- unwrapInsert (UUT.insert ins)
        firstVerify <- TRX.statement () (UUT.verifyToken token)
        secondVerify <- TRX.statement () (UUT.verifyToken token)
        TRX.condemn
        pure (firstVerify, secondVerify)

      assert $ do
        (first', second') <- assertRight result
        _ <- assertJust first'
        assertNothing second'

-- | Insert two tokens, invalidateForUser, both become unverifiable.
prop_invalidateForUser :: TestDBConfig -> PropertyT IO ()
prop_invalidateForUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token1 <- liftIO generateVerificationToken
      token2 <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- unwrapInsert (UUT.insert (mkInsert userId token1 emailText))
        _ <- unwrapInsert (UUT.insert (mkInsert userId token2 emailText))
        TRX.statement () (UUT.invalidateForUser userId)
        v1 <- TRX.statement () (UUT.verifyToken token1)
        v2 <- TRX.statement () (UUT.verifyToken token2)
        TRX.condemn
        pure (v1, v2)

      assert $ do
        (v1, v2) <- assertRight result
        assertNothing v1
        assertNothing v2

-- | Freshly inserted token, getLastTokenCreatedAt within cooldown returns Just.
prop_getLastTokenCreatedAt_withinCooldown :: TestDBConfig -> PropertyT IO ()
prop_getLastTokenCreatedAt_withinCooldown cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- unwrapInsert (UUT.insert (mkInsert userId token emailText))
        mResult <- TRX.statement () (UUT.getLastTokenCreatedAt userId 60)
        TRX.condemn
        pure mResult

      assert $ do
        mId <- assertRight result
        _ <- assertJust mId
        pure ()

-- | Force created_at to past, getLastTokenCreatedAt returns Nothing.
prop_getLastTokenCreatedAt_outsideCooldown :: TestDBConfig -> PropertyT IO ()
prop_getLastTokenCreatedAt_outsideCooldown cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- unwrapInsert (UUT.insert (mkInsert userId token emailText))

        -- Force created_at to well past the cooldown
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE email_verification_tokens
              SET created_at = NOW() - INTERVAL '2 hours'
              WHERE token = #{token}
            |]

        mResult <- TRX.statement () (UUT.getLastTokenCreatedAt userId 60)
        TRX.condemn
        pure mResult

      assert $ do
        mId <- assertRight result
        assertNothing mId

-- | Force token to expired, deleteExpired removes it, verifyToken returns Nothing.
prop_deleteExpired :: TestDBConfig -> PropertyT IO ()
prop_deleteExpired cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- unwrapInsert (UUT.insert (mkInsert userId token emailText))

        -- Force token to be expired
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE email_verification_tokens
              SET expires_at = NOW() - INTERVAL '1 hour'
              WHERE token = #{token}
            |]

        TRX.statement () UUT.deleteExpired
        mResult <- TRX.statement () (UUT.verifyToken token)
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
      token <- liftIO generateVerificationToken
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let emailText = display (UserMetadata.uwmiEmail userWithMetadata)
        _ <- unwrapInsert (UUT.insert (mkInsert userId token emailText))

        -- Force created_at to 31 days ago
        TRX.statement () $
          interp @()
            False
            [sql|
              UPDATE email_verification_tokens
              SET created_at = NOW() - INTERVAL '31 days'
              WHERE token = #{token}
            |]

        TRX.statement () (UUT.deleteOlderThanDays 30)
        mResult <- TRX.statement () (UUT.verifyToken token)
        TRX.condemn
        pure mResult

      assert $ do
        mModel <- assertRight result
        assertNothing mModel

-- | Insert with a non-existent user ID should fail due to FK constraint.
prop_insertNonExistentUser :: TestDBConfig -> PropertyT IO ()
prop_insertNonExistentUser cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      token <- liftIO generateVerificationToken
      let ins = mkInsert (User.Id 999999) token "fake@example.com"
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.statement () (UUT.insert ins)

      assert $ do
        result <== isLeft

-- | Before any verification, isUserEmailVerified returns Nothing.
prop_isUserEmailVerified_beforeVerification :: TestDBConfig -> PropertyT IO ()
prop_isUserEmailVerified_beforeVerification cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        mResult <- TRX.statement () (UUT.isUserEmailVerified userId)
        TRX.condemn
        pure mResult

      assert $ do
        mVerified <- assertRight result
        assertNothing mVerified
