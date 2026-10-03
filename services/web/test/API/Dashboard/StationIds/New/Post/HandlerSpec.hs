module API.Dashboard.StationIds.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.New.Post.Handler (action)
import API.Dashboard.StationIds.New.Post.Route (FormData (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Class (MonadDB (..))
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

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationIds.New.Post.Handler.action" $ do
      it "rejects a station ID longer than 10 seconds" $ \cfg ->
        expectRejected
          cfg
          "station-id-too-long"
          (Just 11)
          "Station IDs can run at most 10 seconds. This file runs 11."
      it "rejects a station ID with no measured length" $ \cfg ->
        expectRejected
          cfg
          "station-id-no-length"
          Nothing
          "Could not read the audio length. Upload the file again."
      it "accepts a station ID within the limit" $ \cfg ->
        expectAccepted cfg "station-id-within-limit" 8

--------------------------------------------------------------------------------

-- | Stage an upload with this length and submit its token. Expect a
-- validation error with this message.
--
-- The length check runs before the claim, so no file needs to be on disk.
expectRejected :: TestDBConfig -> Text -> Maybe Int64 -> Text -> IO ()
expectRejected cfg name durationSeconds message = do
  userInsert <- mkUserInsert name UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _uploadId <- TRX.statement () (StagedUploads.insert (stagedStationId name userModel durationSeconds))
      pure userModel
    userModel <- liftIO $ expectSetupRight dbResult
    let form = FormData {fdTitle = "KPBJ ID", fdAudioToken = name}
    result <- runExceptT (action userModel form)
    liftIO $ case result of
      Left (ValidationError msg) -> msg `shouldBe` message
      other -> expectationFailure $ "Expected a validation error, got " <> show other

-- | Stage an upload with this length, put its file on disk, and submit its
-- token. Expect the station ID to save.
expectAccepted :: TestDBConfig -> Text -> Int64 -> IO ()
expectAccepted cfg name durationSeconds = do
  userInsert <- mkUserInsert name UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _uploadId <- TRX.statement () (StagedUploads.insert (stagedStationId name userModel (Just durationSeconds)))
      pure userModel
    userModel <- liftIO $ expectSetupRight dbResult

    -- The claim moves the staged file, so it has to exist.
    let stagingFilePath = "/tmp/kpbj/" <> Text.unpack (stagingPath name)
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory stagingFilePath)
      writeFile stagingFilePath "fake audio content"

    let form = FormData {fdTitle = "KPBJ ID", fdAudioToken = name}
    result <- runExceptT (action userModel form)
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the station ID to save, got " <> show err

-- | A staged station ID upload as the upload handler writes it.
--
-- The token doubles as the file name, so each case stages its own file.
stagedStationId :: Text -> User.Model -> Maybe Int64 -> StagedUploads.Insert
stagedStationId name userModel durationSeconds =
  StagedUploads.Insert
    { StagedUploads.siToken = StagedUploads.Token name,
      StagedUploads.siUserId = User.mId userModel,
      StagedUploads.siOriginalName = "kpbj-id.mp3",
      StagedUploads.siStoragePath = stagingPath name,
      StagedUploads.siMimeType = "audio/mpeg",
      StagedUploads.siFileSize = 160000,
      StagedUploads.siDurationSeconds = durationSeconds,
      StagedUploads.siUploadType = StagedUploads.StationIdAudio
    }

-- | Where the staged file sits, relative to the local storage root.
stagingPath :: Text -> Text
stagingPath name = "temp/staging/" <> name <> ".mp3"
