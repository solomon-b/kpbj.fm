module Effects.Storage.LocalSpec (spec) where

--------------------------------------------------------------------------------

import Domain.Types.StorageBackend (LocalStorageConfig (..))
import Effects.Storage.Local (deleteFileLocal)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "deleteFileLocal" $ do
  it "deletes a file that exists" $
    withSystemTempDirectory "kpbj-storage" $ \root -> do
      createDirectoryIfMissing True (root </> "temp" </> "staging")
      writeFile (root </> "temp" </> "staging" </> "a.mp3") "audio"
      result <- deleteFileLocal (LocalStorageConfig root) "temp/staging/a.mp3"
      result `shouldBe` Right ()
      doesFileExist (root </> "temp" </> "staging" </> "a.mp3") `shouldReturn` False

  it "gives success for a file that does not exist" $
    withSystemTempDirectory "kpbj-storage" $ \root -> do
      result <- deleteFileLocal (LocalStorageConfig root) "temp/staging/missing.mp3"
      result `shouldBe` Right ()

  it "gives an error when the path is a directory" $
    withSystemTempDirectory "kpbj-storage" $ \root -> do
      createDirectoryIfMissing True (root </> "temp" </> "staging" </> "dir")
      result <- deleteFileLocal (LocalStorageConfig root) "temp/staging/dir"
      result `shouldSatisfy` either (const True) (const False)
