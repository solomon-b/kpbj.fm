module API.Newsletter.Subscribe.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Newsletter.Subscribe.Post.Handler (SubscribeResult (..), action)
import API.Newsletter.Subscribe.Post.Route (SubscribeForm (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.EmailAddress (mkEmailAddress)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Newsletter.Subscribe.Post.Handler" $ do
      describe "action" $ do
        it "returns SubscribeInvalidEmail for malformed email" test_invalidEmail
        it "returns SubscribeSuccess and inserts a new subscriber" test_validEmailInserts
        it "returns SubscribeSuccess on duplicate without inserting twice" test_duplicateEmailNoOp

--------------------------------------------------------------------------------

test_invalidEmail :: TestDBConfig -> IO ()
test_invalidEmail cfg = bracketAppM cfg $ do
  let form = SubscribeForm {sfEmail = mkEmailAddress "not-an-email"}
  result <- runExceptT (action form)
  liftIO $ case result of
    Right SubscribeInvalidEmail -> pure ()
    other -> expectationFailure $ "Expected Right SubscribeInvalidEmail, got " <> show other

test_validEmailInserts :: TestDBConfig -> IO ()
test_validEmailInserts cfg = bracketAppM cfg $ do
  let email = mkEmailAddress "fresh-subscriber@test.example.com"
  let form = SubscribeForm {sfEmail = email}
  result <- runExceptT (action form)
  case result of
    Right SubscribeSuccess -> do
      countResult <-
        runDB $
          TRX.transaction TRX.ReadCommitted TRX.Read $
            TRX.statement () (NewsletterSubscribers.countByEmail email)
      liftIO $ case countResult of
        Right c -> c `shouldBe` 1
        Left e -> expectationFailure $ "DB error: " <> show e
    other -> liftIO . expectationFailure $ "Expected Right SubscribeSuccess, got " <> show other

test_duplicateEmailNoOp :: TestDBConfig -> IO ()
test_duplicateEmailNoOp cfg = bracketAppM cfg $ do
  let email = mkEmailAddress "dup-subscriber@test.example.com"
  let form = SubscribeForm {sfEmail = email}
  _ <- runExceptT (action form)
  result <- runExceptT (action form)
  case result of
    Right SubscribeSuccess -> do
      countResult <-
        runDB $
          TRX.transaction TRX.ReadCommitted TRX.Read $
            TRX.statement () (NewsletterSubscribers.countByEmail email)
      liftIO $ case countResult of
        Right c -> c `shouldBe` 1
        Left e -> expectationFailure $ "DB error: " <> show e
    other -> liftIO . expectationFailure $ "Expected Right SubscribeSuccess, got " <> show other
