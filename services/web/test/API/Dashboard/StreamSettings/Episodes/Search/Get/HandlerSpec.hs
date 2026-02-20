module API.Dashboard.StreamSettings.Episodes.Search.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StreamSettings.Episodes.Search.Get.Handler (action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StreamSettings.Episodes.Search.Get.Handler" $ do
      describe "action" $ do
        it "empty query returns Right" test_emptyQueryReturnsRight
        it "blank query returns Right" test_blankQueryReturnsRight
        it "non-matching query returns Right" test_nonMatchingQueryReturnsRight

--------------------------------------------------------------------------------

-- | Nothing query short-circuits to mempty without hitting the database.
test_emptyQueryReturnsRight :: TestDBConfig -> IO ()
test_emptyQueryReturnsRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action Nothing
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

-- | A blank string query is treated the same as an empty query.
test_blankQueryReturnsRight :: TestDBConfig -> IO ()
test_blankQueryReturnsRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (Just "")
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

-- | A query with no matching episodes returns Right with an empty-results fragment.
test_nonMatchingQueryReturnsRight :: TestDBConfig -> IO ()
test_nonMatchingQueryReturnsRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (Just "xyznonexistent12345")
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
