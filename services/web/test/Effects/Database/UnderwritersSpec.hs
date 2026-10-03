-- | Tests for the @underwriters@ table queries.
module Effects.Database.UnderwritersSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Underwriters" $ do
      it "lists underwriters by name" listsByName
      it "renames an underwriter" renames

listsByName :: TestDBConfig -> IO ()
listsByName cfg =
  bracketAppM cfg $ do
    _ <- execQuery (Underwriters.insertUnderwriter "Sal's Records")
    _ <- execQuery (Underwriters.insertUnderwriter "Ace Hardware")
    result <- execQuery Underwriters.getAll
    liftIO $ case result of
      Left err -> expectationFailure (show err)
      Right rows -> map (.uwName) rows `shouldBe` ["Ace Hardware", "Sal's Records"]

renames :: TestDBConfig -> IO ()
renames cfg =
  bracketAppM cfg $ do
    inserted <- execQuery (Underwriters.insertUnderwriter "Sal's Records")
    case inserted of
      Right (Just uwId) -> do
        renamed <- execQuery (Underwriters.renameUnderwriter uwId "Sal's Records & Tapes")
        liftIO $ fmap (fmap (.uwName)) renamed `shouldBe` Right (Just "Sal's Records & Tapes")
      other -> liftIO $ expectationFailure ("insert failed: " <> show other)
