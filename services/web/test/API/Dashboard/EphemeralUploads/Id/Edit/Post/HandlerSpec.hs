{-# LANGUAGE PackageImports #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Post.Handler (action)
import API.Dashboard.EphemeralUploads.Id.Edit.Post.Route (FormData (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
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
    describe "API.Dashboard.EphemeralUploads.Id.Edit.Post.Handler.action" $ do
      it "stores the length of replacement audio" test_storesReplacementLength
      it "keeps the length when the audio does not change" test_keepsLengthWithoutNewAudio
      it "rejects replacement audio with no measured length" test_rejectsNoLength

--------------------------------------------------------------------------------

test_storesReplacementLength :: TestDBConfig -> IO ()
test_storesReplacementLength cfg = do
  let name = "eu-edit-replacement"
  userInsert <- mkUserInsert name UserMetadata.Staff
  bracketAppM cfg $ do
    (userModel, userMetaModel, ephemeralId) <- setup name 600 (Just (name, Just 95)) userInsert

    -- The claim moves the staged file, so it has to exist.
    let stagingFilePath = "/tmp/kpbj/" <> Text.unpack (stagingPath name)
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory stagingFilePath)
      writeFile stagingFilePath "fake audio content"

    result <- runExceptT (action userModel userMetaModel ephemeralId (form name name))
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the edit to save, got " <> show err

    row <- fetchRow ephemeralId
    liftIO $ EphemeralUploads.eumDurationSeconds row `shouldBe` 95

test_keepsLengthWithoutNewAudio :: TestDBConfig -> IO ()
test_keepsLengthWithoutNewAudio cfg = do
  let name = "eu-edit-keeps-length"
  userInsert <- mkUserInsert name UserMetadata.Staff
  bracketAppM cfg $ do
    (userModel, userMetaModel, ephemeralId) <- setup name 600 Nothing userInsert

    result <- runExceptT (action userModel userMetaModel ephemeralId (form name ""))
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the edit to save, got " <> show err

    row <- fetchRow ephemeralId
    liftIO $ EphemeralUploads.eumDurationSeconds row `shouldBe` 600

test_rejectsNoLength :: TestDBConfig -> IO ()
test_rejectsNoLength cfg = do
  let name = "eu-edit-no-length"
  userInsert <- mkUserInsert name UserMetadata.Staff
  bracketAppM cfg $ do
    -- The length check runs before the claim, so no file needs to be on disk.
    (userModel, userMetaModel, ephemeralId) <- setup name 600 (Just (name, Nothing)) userInsert

    result <- runExceptT (action userModel userMetaModel ephemeralId (form name name))
    liftIO $ case result of
      Left (ValidationError msg) ->
        msg `shouldBe` "Could not read the audio length. Upload the file again."
      other -> expectationFailure $ "Expected a validation error, got " <> show other

    row <- fetchRow ephemeralId
    liftIO $ do
      EphemeralUploads.eumAudioFilePath row `shouldBe` originalPath name
      EphemeralUploads.eumDurationSeconds row `shouldBe` 600

--------------------------------------------------------------------------------

-- | The form for an edit. The token is empty when the audio does not change.
form :: Text -> Text -> FormData
form name audioToken =
  FormData
    { fdTitle = name,
      fdDescription = description,
      fdAudioToken = audioToken
    }

-- | A description that passes the 80 character minimum.
description :: Text
description =
  "A short clip for the tests. It has two sentences so that it passes the minimum length check."

-- | Create the user, the existing ephemeral upload, and optionally a staged
-- replacement with this token and length.
setup ::
  Text ->
  Int64 ->
  Maybe (Text, Maybe Int64) ->
  UserMetadata.UserWithMetadataInsert ->
  AppM (User.Model, UserMetadata.Model, EphemeralUploads.Id)
setup name existingLength staged userInsert = do
  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (userModel, userMetaModel) <- setupUserModels userInsert
    mEphemeralId <- TRX.statement () (EphemeralUploads.insertEphemeralUpload (existingUpload name existingLength userModel))
    case staged of
      Nothing -> pure ()
      Just (token, stagedLength) -> do
        _uploadId <- TRX.statement () (StagedUploads.insert (stagedEphemeral token userModel stagedLength))
        pure ()
    pure (userModel, userMetaModel, mEphemeralId)
  (userModel, userMetaModel, mEphemeralId) <- liftIO $ expectSetupRight dbResult
  case mEphemeralId of
    Just ephemeralId -> pure (userModel, userMetaModel, ephemeralId)
    Nothing -> liftIO $ fail "Could not insert the ephemeral upload."

-- | Read the ephemeral upload back.
fetchRow :: EphemeralUploads.Id -> AppM EphemeralUploads.Model
fetchRow ephemeralId = do
  dbResult <- execQuery (EphemeralUploads.getEphemeralUploadById ephemeralId)
  mRow <- liftIO $ expectSetupRight dbResult
  case mRow of
    Just row -> pure row
    Nothing -> liftIO $ fail "The ephemeral upload is missing."

-- | The ephemeral upload that exists before the edit.
existingUpload :: Text -> Int64 -> User.Model -> EphemeralUploads.Insert
existingUpload name existingLength userModel =
  EphemeralUploads.Insert
    { EphemeralUploads.euiTitle = name,
      EphemeralUploads.euiDescription = description,
      EphemeralUploads.euiAudioFilePath = originalPath name,
      EphemeralUploads.euiMimeType = "audio/mpeg",
      EphemeralUploads.euiFileSize = 1024,
      EphemeralUploads.euiDurationSeconds = existingLength,
      EphemeralUploads.euiCreatorId = User.mId userModel
    }

-- | The audio path of the existing ephemeral upload.
originalPath :: Text -> Text
originalPath name = "audio/ephemeral-uploads/original-" <> name <> ".mp3"

-- | A staged ephemeral upload as the upload handler writes it.
--
-- The token doubles as the file name, so each case stages its own file.
stagedEphemeral :: Text -> User.Model -> Maybe Int64 -> StagedUploads.Insert
stagedEphemeral token userModel durationSeconds =
  StagedUploads.Insert
    { StagedUploads.siToken = StagedUploads.Token token,
      StagedUploads.siUserId = User.mId userModel,
      StagedUploads.siOriginalName = "clip.mp3",
      StagedUploads.siStoragePath = stagingPath token,
      StagedUploads.siMimeType = "audio/mpeg",
      StagedUploads.siFileSize = 160000,
      StagedUploads.siDurationSeconds = durationSeconds,
      StagedUploads.siUploadType = StagedUploads.EphemeralAudio
    }

-- | Where the staged file sits, relative to the local storage root.
stagingPath :: Text -> Text
stagingPath token = "temp/staging/" <> token <> ".mp3"
