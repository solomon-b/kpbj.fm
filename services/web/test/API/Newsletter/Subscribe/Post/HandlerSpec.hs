module API.Newsletter.Subscribe.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Newsletter.Subscribe.Post.Handler (SubscribeResult (..), action)
import API.Newsletter.Subscribe.Post.Route (SubscribeForm (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Gen.EmailAddress (mkEmailAddressUnsafe)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Web.FormUrlEncoded (fromForm, toForm)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Newsletter.Subscribe.Post.Handler" $ do
      describe "form decoder" $ do
        it "refuses a malformed email with a message for the user" $ \_cfg ->
          test_invalidEmail
      describe "action" $ do
        it "returns SubscribeSuccess and inserts a new subscriber" test_validEmailInserts
        it "returns SubscribeSuccess on duplicate without inserting twice" test_duplicateEmailNoOp

--------------------------------------------------------------------------------

test_invalidEmail :: IO ()
test_invalidEmail =
  case decoded of
    Left message -> message `shouldBe` "Please enter a valid email address."
    Right form -> expectationFailure $ "Expected a decode error, got " <> show form
  where
    decoded :: Either Text SubscribeForm
    decoded = fromForm (toForm [("email" :: Text, "not-an-email" :: Text)])

test_validEmailInserts :: TestDBConfig -> IO ()
test_validEmailInserts cfg = bracketAppM cfg $ do
  let email = mkEmailAddressUnsafe "fresh-subscriber@test.example.com"
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
  let email = mkEmailAddressUnsafe "dup-subscriber@test.example.com"
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
