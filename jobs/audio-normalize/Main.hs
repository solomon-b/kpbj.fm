module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Options.Applicative
import System.Directory (copyFile, doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (replaceExtension, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed qualified as Proc
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Configuration

data Options = Options
  { optDryRun :: Bool,
    optMaxRetries :: Int64,
    optLimit :: Int64
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "List episodes that would be processed without processing them")
    <*> option auto (long "max-retries" <> value 3 <> showDefault <> help "Max processing attempts per episode")
    <*> option auto (long "limit" <> value 50 <> showDefault <> help "Max episodes to process per run")

data StorageMode
  = -- | S3 storage: bucket name, region, endpoint URL
    S3Storage Text Text Text
  | -- | Local storage: root directory path
    LocalStorage FilePath

--------------------------------------------------------------------------------
-- Types

data EpisodeToProcess = EpisodeToProcess
  { epId :: Int64,
    epAudioFilePath :: Text
  }

--------------------------------------------------------------------------------
-- Loudnorm measured values parsed from ffmpeg pass 1

data LoudnormMeasured = LoudnormMeasured
  { lmInputI :: Text,
    lmInputTp :: Text,
    lmInputLra :: Text,
    lmInputThresh :: Text,
    lmTargetOffset :: Text
  }

instance Aeson.FromJSON LoudnormMeasured where
  parseJSON = Aeson.withObject "LoudnormMeasured" $ \o ->
    LoudnormMeasured
      <$> o .: "input_i"
      <*> o .: "input_tp"
      <*> o .: "input_lra"
      <*> o .: "input_thresh"
      <*> o .: "target_offset"

--------------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Normalize audio loudness for unprocessed episodes"))
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mDbUrl of
    Nothing -> do
      hPutStrLn stderr "ERROR: DATABASE_URL environment variable is required"
      exitFailure
    Just dbUrl -> do
      storage <- detectStorageMode
      connResult <- Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
      case connResult of
        Left err -> do
          hPutStrLn stderr $ "ERROR: Failed to connect to database: " <> show err
          exitFailure
        Right conn ->
          bracket (pure conn) Connection.release $ \c ->
            if optDryRun opts
              then runDryRun c opts
              else runProcessing c opts storage

-- | Detect storage mode from environment variables.
--
-- If BUCKET_NAME is set, use S3 (via aws CLI). Otherwise, use local storage
-- at @/tmp/kpbj@ (same default as the web service's @defaultLocalConfig@).
detectStorageMode :: IO StorageMode
detectStorageMode = do
  mBucket <- lookupEnv "BUCKET_NAME"
  case mBucket of
    Just bucket -> do
      region <- maybe "us-east-1" id <$> lookupEnv "AWS_REGION"
      endpoint <- maybe "" id <$> lookupEnv "AWS_ENDPOINT_URL_S3"
      pure $ S3Storage (Text.pack bucket) (Text.pack region) (Text.pack endpoint)
    Nothing -> pure $ LocalStorage "/tmp/kpbj"

--------------------------------------------------------------------------------
-- Dry run

runDryRun :: Connection.Connection -> Options -> IO ()
runDryRun conn opts = do
  hPutStrLn stderr "[audio-normalize] DRY RUN — listing episodes that would be processed"
  result <- Session.run (fetchEpisodes (optMaxRetries opts) (optLimit opts)) conn
  case result of
    Left err -> hPutStrLn stderr $ "[audio-normalize] Query failed: " <> show err
    Right episodes -> do
      hPutStrLn stderr $ "[audio-normalize] Found " <> show (length episodes) <> " episodes to process"
      mapM_ (\ep -> hPutStrLn stderr $ "  id=" <> show (epId ep) <> " path=" <> Text.unpack (epAudioFilePath ep)) episodes

--------------------------------------------------------------------------------
-- Processing loop

runProcessing :: Connection.Connection -> Options -> StorageMode -> IO ()
runProcessing conn opts storage = do
  hPutStrLn stderr "[audio-normalize] Starting audio normalization batch"
  result <- Session.run (fetchEpisodes (optMaxRetries opts) (optLimit opts)) conn
  case result of
    Left err -> do
      hPutStrLn stderr $ "[audio-normalize] Query failed: " <> show err
      exitFailure
    Right episodes -> do
      hPutStrLn stderr $ "[audio-normalize] Found " <> show (length episodes) <> " episodes to process"
      mapM_ (processOneEpisode conn storage) episodes
      hPutStrLn stderr "[audio-normalize] Batch complete"

processOneEpisode :: Connection.Connection -> StorageMode -> EpisodeToProcess -> IO ()
processOneEpisode conn storage ep = do
  hPutStrLn stderr $ "[audio-normalize] Processing episode " <> show (epId ep)

  -- Atomic claim: prevents double-processing
  claimResult <- Session.run (claimEpisode (epId ep)) conn
  case claimResult of
    Left err -> hPutStrLn stderr $ "[audio-normalize] Failed to claim episode " <> show (epId ep) <> ": " <> show err
    Right claimed
      | not claimed -> hPutStrLn stderr $ "[audio-normalize] Episode " <> show (epId ep) <> " already claimed, skipping"
      | otherwise -> do
          result <- try $ withSystemTempDirectory "audio-normalize" $ \tmpDir ->
            processEpisodeInTempDir conn storage ep tmpDir
          case result of
            Left (e :: SomeException) -> do
              hPutStrLn stderr $ "[audio-normalize] Episode " <> show (epId ep) <> " failed: " <> show e
              markFailed conn (epId ep)
            Right (Left err) -> do
              hPutStrLn stderr $ "[audio-normalize] Episode " <> show (epId ep) <> " failed: " <> Text.unpack err
              markFailed conn (epId ep)
            Right (Right ()) ->
              hPutStrLn stderr $ "[audio-normalize] Episode " <> show (epId ep) <> " processed successfully"

processEpisodeInTempDir :: Connection.Connection -> StorageMode -> EpisodeToProcess -> FilePath -> IO (Either Text ())
processEpisodeInTempDir conn storage ep tmpDir = do
  let audioKey = epAudioFilePath ep
      inputPath = tmpDir </> "input" <> takeExtension (Text.unpack audioKey)

  -- Download audio file
  downloadResult <- downloadFile storage audioKey inputPath
  case downloadResult of
    Left err -> pure $ Left $ "Download failed: " <> err
    Right () -> do
      -- Two-pass loudnorm + compress + limit
      processResult <- processAudioFile inputPath tmpDir
      case processResult of
        Left err -> pure $ Left err
        Right outputPath -> do
          -- Determine output key (replace extension with .mp3, avoid overwriting original)
          let newKey = makeOutputKey audioKey
          -- Upload processed file
          uploadResult <- uploadFile storage newKey outputPath
          case uploadResult of
            Left err -> pure $ Left $ "Upload failed: " <> err
            Right () -> do
              -- Get processed file size
              fileSize <- getFileSize outputPath
              -- Update database
              updateResult <- Session.run (markProcessed (epId ep) audioKey newKey (fromIntegral fileSize)) conn
              case updateResult of
                Left err -> pure $ Left $ "DB update failed: " <> Text.pack (show err)
                Right () -> pure $ Right ()

--------------------------------------------------------------------------------
-- File path strategy

-- | Generate the output key for the processed MP3.
--
-- Replaces the file extension with @.mp3@. If the file is already @.mp3@,
-- appends @_normalized@ before the extension to avoid overwriting the original.
makeOutputKey :: Text -> Text
makeOutputKey key =
  let path = Text.unpack key
      ext = takeExtension path
   in if ext == ".mp3"
        then Text.pack $ replaceExtension path "_normalized.mp3"
        else Text.pack $ replaceExtension path ".mp3"

--------------------------------------------------------------------------------
-- File download/upload

downloadFile :: StorageMode -> Text -> FilePath -> IO (Either Text ())
downloadFile (LocalStorage root) key destPath = do
  let srcPath = root </> Text.unpack key
  exists <- doesFileExist srcPath
  if not exists
    then pure $ Left $ "Local file not found: " <> Text.pack srcPath
    else do
      copyFile srcPath destPath
      pure $ Right ()
downloadFile (S3Storage bucket _region endpoint) key destPath = do
  let s3Uri = "s3://" <> Text.unpack bucket <> "/" <> Text.unpack key
      args =
        ["s3", "cp", s3Uri, destPath]
          <> (if Text.null endpoint then [] else ["--endpoint-url", Text.unpack endpoint])
      proc = Proc.proc "aws" args
  (exitCode, _stdout, stderrBs) <- Proc.readProcess proc
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code -> pure $ Left $ "aws s3 cp failed (code " <> Text.pack (show code) <> "): " <> decodeStderr stderrBs

uploadFile :: StorageMode -> Text -> FilePath -> IO (Either Text ())
uploadFile (LocalStorage root) key srcPath = do
  let destPath = root </> Text.unpack key
  result <- try @SomeException $ copyFile srcPath destPath
  case result of
    Left err -> pure $ Left $ "Local copy failed: " <> Text.pack (show err)
    Right () -> pure $ Right ()
uploadFile (S3Storage bucket _region endpoint) key srcPath = do
  let s3Uri = "s3://" <> Text.unpack bucket <> "/" <> Text.unpack key
      args =
        ["s3", "cp", srcPath, s3Uri, "--content-type", "audio/mpeg", "--acl", "public-read"]
          <> (if Text.null endpoint then [] else ["--endpoint-url", Text.unpack endpoint])
      proc = Proc.proc "aws" args
  (exitCode, _stdout, stderrBs) <- Proc.readProcess proc
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code -> pure $ Left $ "aws s3 cp upload failed (code " <> Text.pack (show code) <> "): " <> decodeStderr stderrBs

--------------------------------------------------------------------------------
-- Audio processing (two-pass loudnorm + compression + limiting)

-- | Process an audio file: normalize loudness, compress, limit, transcode to MP3.
--
-- Uses a two-pass loudnorm workflow:
--
-- 1. @ffmpeg@ pass 1 analyzes loudness (dry run to @\/dev\/null@)
-- 2. @ffmpeg@ pass 2 encodes with measured loudness correction
--
-- Filter chain mirrors @radio.liq@:
--
-- @
-- loudnorm    → normalize to -14 LUFS (two-pass, linear mode)
-- acompressor → threshold=-10dB, ratio=3:1, attack=10ms, release=200ms, makeup=3dB
-- alimiter    → limit=-1dB, attack=5ms, release=50ms
-- @
processAudioFile :: FilePath -> FilePath -> IO (Either Text FilePath)
processAudioFile inputPath tmpDir = do
  -- Pass 1: analyze loudness
  analysisResult <- analyzeLoudness inputPath
  case analysisResult of
    Left err -> pure $ Left err
    Right measured -> do
      -- Pass 2: encode with measured values
      let outputPath = tmpDir </> "output.mp3"
      encodeResult <- encodeLoudnorm inputPath measured outputPath
      case encodeResult of
        Left err -> pure $ Left err
        Right () -> pure $ Right outputPath

-- | Run ffmpeg loudnorm pass 1 to measure loudness characteristics.
analyzeLoudness :: FilePath -> IO (Either Text LoudnormMeasured)
analyzeLoudness inputPath = do
  let proc =
        Proc.proc
          "ffmpeg"
          [ "-i",
            inputPath,
            "-af",
            "loudnorm=I=-14:TP=-1:LRA=11:print_format=json",
            "-f",
            "null",
            "/dev/null"
          ]
  result <- timeout (300 * 1000000) (Proc.readProcess proc)
  case result of
    Nothing -> pure $ Left "ffmpeg loudnorm analysis timed out"
    Just (ExitFailure code, _, stderrBs) ->
      pure $ Left $ "ffmpeg pass 1 exited with code " <> Text.pack (show code) <> ": " <> decodeStderr stderrBs
    Just (ExitSuccess, _, stderrBs) ->
      case parseLoudnormJson stderrBs of
        Nothing -> pure $ Left $ "Failed to parse loudnorm JSON. stderr tail: " <> decodeStderr stderrBs
        Just measured -> pure $ Right measured

-- | Run ffmpeg loudnorm pass 2 to encode with measured loudness values.
encodeLoudnorm :: FilePath -> LoudnormMeasured -> FilePath -> IO (Either Text ())
encodeLoudnorm inputPath measured outputPath = do
  let af =
        "loudnorm=I=-14:TP=-1:LRA=11"
          <> ":measured_I="
          <> Text.unpack (lmInputI measured)
          <> ":measured_TP="
          <> Text.unpack (lmInputTp measured)
          <> ":measured_LRA="
          <> Text.unpack (lmInputLra measured)
          <> ":measured_thresh="
          <> Text.unpack (lmInputThresh measured)
          <> ":offset="
          <> Text.unpack (lmTargetOffset measured)
          <> ":linear=true"
          <> ",acompressor=threshold=-10dB:ratio=3:attack=10:release=200:makeup=3dB"
          <> ",alimiter=limit=-1dB:attack=5:release=50"
      proc =
        Proc.proc
          "ffmpeg"
          [ "-i",
            inputPath,
            "-af",
            af,
            "-ar",
            "44100",
            "-b:a",
            "192k",
            "-f",
            "mp3",
            "-y",
            outputPath
          ]
  result <- timeout (600 * 1000000) (Proc.readProcess proc)
  case result of
    Nothing -> pure $ Left "ffmpeg encoding timed out"
    Just (ExitFailure code, _, stderrBs) ->
      pure $ Left $ "ffmpeg pass 2 exited with code " <> Text.pack (show code) <> ": " <> decodeStderr stderrBs
    Just (ExitSuccess, _, _) -> pure $ Right ()

-- | Extract the JSON block from ffmpeg's loudnorm output on stderr.
parseLoudnormJson :: LBS.ByteString -> Maybe LoudnormMeasured
parseLoudnormJson stderrBytes =
  let stderrText = TE.decodeUtf8Lenient (LBS.toStrict stderrBytes)
   in case Text.breakOnEnd "{" stderrText of
        ("", _) -> Nothing
        (_, after) ->
          case Text.breakOn "}" after of
            (_, "") -> Nothing
            (content, _) ->
              Aeson.decodeStrict (TE.encodeUtf8 ("{" <> content <> "}"))

--------------------------------------------------------------------------------
-- Database queries

fetchEpisodes :: Int64 -> Int64 -> Session.Session [EpisodeToProcess]
fetchEpisodes maxRetries limit =
  Session.statement (maxRetries, limit) $
    Statement.Statement
      "SELECT id, audio_file_path FROM episodes \
      \WHERE audio_processing_status IN ('unprocessed', 'failed') \
      \AND audio_processing_attempts < $1 \
      \AND audio_file_path IS NOT NULL \
      \AND deleted_at IS NULL \
      \ORDER BY created_at ASC \
      \LIMIT $2"
      ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      )
      (Decoders.rowList episodeDecoder)
      True
  where
    episodeDecoder =
      EpisodeToProcess
        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)

claimEpisode :: Int64 -> Session.Session Bool
claimEpisode episodeId =
  Session.statement episodeId $
    Statement.Statement
      "UPDATE episodes \
      \SET audio_processing_status = 'processing', \
      \    audio_processing_attempts = audio_processing_attempts + 1 \
      \WHERE id = $1 \
      \AND audio_processing_status IN ('unprocessed', 'failed') \
      \RETURNING id"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      (fmap (not . null) (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))))
      True

markProcessed :: Int64 -> Text -> Text -> Int64 -> Session.Session ()
markProcessed episodeId originalPath newPath fileSize =
  Session.statement (episodeId, originalPath, newPath, fileSize) $
    Statement.Statement
      "UPDATE episodes \
      \SET audio_processing_status = 'processed', \
      \    original_audio_file_path = $2, \
      \    audio_file_path = $3, \
      \    audio_mime_type = 'audio/mpeg', \
      \    audio_file_size = $4 \
      \WHERE id = $1"
      ( ((\(a, _, _, _) -> a) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
          <> ((\(_, b, _, _) -> b) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> ((\(_, _, c, _) -> c) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> ((\(_, _, _, d) -> d) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      )
      Decoders.noResult
      True

markFailed :: Connection.Connection -> Int64 -> IO ()
markFailed conn episodeId = do
  result <- Session.run (markFailedSession episodeId) conn
  case result of
    Left err -> hPutStrLn stderr $ "[audio-normalize] Failed to mark episode " <> show episodeId <> " as failed: " <> show err
    Right () -> pure ()

markFailedSession :: Int64 -> Session.Session ()
markFailedSession episodeId =
  Session.statement episodeId $
    Statement.Statement
      "UPDATE episodes SET audio_processing_status = 'failed' WHERE id = $1"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      Decoders.noResult
      True

--------------------------------------------------------------------------------
-- Helpers

-- | Decode stderr bytes to text, taking the last 500 characters for log messages.
decodeStderr :: LBS.ByteString -> Text
decodeStderr bs =
  let t = TE.decodeUtf8Lenient (LBS.toStrict bs)
   in Text.takeEnd 500 t
