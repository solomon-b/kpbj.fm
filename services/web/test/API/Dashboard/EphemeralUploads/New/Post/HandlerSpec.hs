{-# LANGUAGE PackageImports #-}

module API.Dashboard.EphemeralUploads.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Post.Handler (action)
import API.Dashboard.EphemeralUploads.New.Post.Route (FormData (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.EphemeralUploads.New.Post.Handler.action" $ do
      it "stores the measured length" test_storesMeasuredLength
      it "rejects an upload with no measured length" test_rejectsNoLength

--------------------------------------------------------------------------------

test_storesMeasuredLength :: TestDBConfig -> IO ()
test_storesMeasuredLength cfg = do
  let name = "eu-new-stores-length"
  userInsert <- mkUserInsert name UserMetadata.Host
  bracketAppM cfg $ do
    userModel <- stageUpload name (Just 1830) userInsert

    -- The claim moves the staged file, so it has to exist.
    let stagingFilePath = "/tmp/kpbj/" <> Text.unpack (stagingPath name)
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory stagingFilePath)
      writeFile stagingFilePath "fake audio content"

    result <- runExceptT (action userModel (form name))
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the upload to save, got " <> show err

    rows <- findRowsByTitle name
    liftIO $ map EphemeralUploads.euwcDurationSeconds rows `shouldBe` [Just 1830]

test_rejectsNoLength :: TestDBConfig -> IO ()
test_rejectsNoLength cfg = do
  let name = "eu-new-no-length"
  userInsert <- mkUserInsert name UserMetadata.Host
  bracketAppM cfg $ do
    -- The length check runs before the claim, so no file needs to be on disk.
    userModel <- stageUpload name Nothing userInsert

    result <- runExceptT (action userModel (form name))
    liftIO $ case result of
      Left (ValidationError msg) ->
        msg `shouldBe` "Could not read the audio length. Upload the file again."
      other -> expectationFailure $ "Expected a validation error, got " <> show other

    rows <- findRowsByTitle name
    liftIO $ length rows `shouldBe` 0

--------------------------------------------------------------------------------

-- | The form for an upload. The token and the title both use the case name.
form :: Text -> FormData
form name =
  FormData
    { fdTitle = name,
      fdDescription = description,
      fdAudioToken = name
    }

-- | A description that passes the 80 character minimum.
description :: Text
description =
  "A short clip for the tests. It has two sentences so that it passes the minimum length check."

-- | Create the user and stage an upload with this length.
stageUpload ::
  Text ->
  Maybe Int64 ->
  UserMetadata.UserWithMetadataInsert ->
  AppM User.Model
stageUpload name durationSeconds userInsert = do
  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (userModel, _) <- setupUserModels userInsert
    _uploadId <- TRX.statement () (StagedUploads.insert (stagedEphemeral name userModel durationSeconds))
    pure userModel
  liftIO $ expectSetupRight dbResult

-- | Find the ephemeral uploads with this title.
findRowsByTitle :: Text -> AppM [EphemeralUploads.EphemeralUploadWithCreator]
findRowsByTitle title = do
  dbResult <- execQuery (EphemeralUploads.getAllEphemeralUploads True (Limit 1000) (Offset 0))
  rows <- liftIO $ expectSetupRight dbResult
  pure (filter (\row -> EphemeralUploads.euwcTitle row == title) rows)

-- | A staged ephemeral upload as the upload handler writes it.
--
-- The token doubles as the file name, so each case stages its own file.
stagedEphemeral :: Text -> User.Model -> Maybe Int64 -> StagedUploads.Insert
stagedEphemeral name userModel durationSeconds =
  StagedUploads.Insert
    { StagedUploads.siToken = StagedUploads.Token name,
      StagedUploads.siUserId = User.mId userModel,
      StagedUploads.siOriginalName = "clip.mp3",
      StagedUploads.siStoragePath = stagingPath name,
      StagedUploads.siMimeType = "audio/mpeg",
      StagedUploads.siFileSize = 160000,
      StagedUploads.siDurationSeconds = durationSeconds,
      StagedUploads.siUploadType = StagedUploads.EphemeralAudio
    }

-- | Where the staged file sits, relative to the local storage root.
stagingPath :: Text -> Text
stagingPath name = "temp/staging/" <> name <> ".mp3"
