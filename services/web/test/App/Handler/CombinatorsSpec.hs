-- | Integration tests for "App.Handler.Combinators".
--
-- The role gates of "App.Auth.Roles" check roles and suspension, and
-- "App.Auth.GateSpec" tests them with 'App.Handler.Combinators.authorizeShow'.
-- This module tests the validation helpers.
module App.Handler.CombinatorsSpec (spec) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireJust, requireRight)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "App.Handler.Combinators" $ do
      describe "requireJust" $ do
        it "returns the inner value when given Just" test_requireJust_succeedsForJust
        it "fails with ValidationError when given Nothing" test_requireJust_failsForNothing

      describe "requireRight" $ do
        it "returns the inner value when given Right" test_requireRight_succeedsForRight
        it "fails with ValidationError when given Left" test_requireRight_failsForLeft

--------------------------------------------------------------------------------
-- requireJust

-- | Providing a Just value returns the unwrapped value.
test_requireJust_succeedsForJust :: TestDBConfig -> IO ()
test_requireJust_succeedsForJust cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireJust "error msg" (Just (42 :: Int))

    liftIO $ case result of
      Right 42 -> pure ()
      Right v -> expectationFailure $ "Expected Right 42 but got Right " <> show v
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err

-- | Providing Nothing throws a ValidationError.
test_requireJust_failsForNothing :: TestDBConfig -> IO ()
test_requireJust_failsForNothing cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireJust "error msg" (Nothing :: Maybe Int)

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

--------------------------------------------------------------------------------
-- requireRight

-- | Providing a Right value returns the unwrapped value.
test_requireRight_succeedsForRight :: TestDBConfig -> IO ()
test_requireRight_succeedsForRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireRight id (Right (99 :: Int) :: Either Text Int)

    liftIO $ case result of
      Right 99 -> pure ()
      Right v -> expectationFailure $ "Expected Right 99 but got Right " <> show v
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err

-- | Providing a Left value throws a ValidationError carrying the converted message.
test_requireRight_failsForLeft :: TestDBConfig -> IO ()
test_requireRight_failsForLeft cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ requireRight id (Left "something went wrong" :: Either Text Int)

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"
