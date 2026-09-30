module API.Dashboard.Underwriters.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriters.Form (UnderwriterForm (..))
import API.Dashboard.Underwriters.New.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Underwriters.New.Post.Handler.action" $ do
      it "rejects a blank name" test_rejectsBlankName
      it "adds the underwriter and returns the list" test_addsUnderwriter

--------------------------------------------------------------------------------

test_rejectsBlankName :: TestDBConfig -> IO ()
test_rejectsBlankName cfg = bracketAppM cfg $ do
  result <- runExceptT (action (UnderwriterForm "   "))
  liftIO $ case result of
    Left (ValidationError msg) -> msg `shouldBe` "A name is required."
    other -> expectationFailure $ "Expected a validation error, got " <> show other

test_addsUnderwriter :: TestDBConfig -> IO ()
test_addsUnderwriter cfg = bracketAppM cfg $ do
  result <- runExceptT (action (UnderwriterForm "  Sun Valley Hardware "))
  liftIO $ case result of
    Right underwriters -> map Underwriters.uwName underwriters `shouldBe` ["Sun Valley Hardware"]
    Left err -> expectationFailure $ "Expected the list, got " <> show err
