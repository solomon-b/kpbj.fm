{-# LANGUAGE QuasiQuotes #-}

-- | Audio duration extraction via ffprobe.
--
-- Extracts duration metadata from audio files without transcoding.
-- Full loudness normalization and processing is handled by the
-- @audio-normalize@ nightly batch job.
--
-- Requires @ffprobe@ on PATH.
module Effects.AudioProcessing
  ( extractDuration,
    DurationExtractionError (..),
    displayDurationError,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Exit (ExitCode (..))
import System.Process.Typed qualified as Proc
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Types

-- | Errors that can occur during duration extraction.
newtype DurationExtractionError
  = -- | ffprobe failed to extract duration from the input file.
    DurationExtractionFailed Text
  deriving stock (Show)

-- | User-facing error message for a duration extraction error.
displayDurationError :: DurationExtractionError -> Text
displayDurationError = \case
  DurationExtractionFailed _ -> "Failed to analyze audio file duration"

--------------------------------------------------------------------------------
-- Public API

-- | Extract duration from an audio file using ffprobe.
--
-- Returns the duration in seconds, rounded to the nearest integer.
-- This is a fast operation (~1-2 seconds) that only reads file metadata.
extractDuration :: FilePath -> IO (Either DurationExtractionError Int64)
extractDuration inputPath = do
  let proc =
        Proc.proc
          "ffprobe"
          [ "-v",
            "quiet",
            "-print_format",
            "json",
            "-show_format",
            inputPath
          ]
  result <- timeout (120 * 1000000) (Proc.readProcess proc)
  case result of
    Nothing -> pure $ Left $ DurationExtractionFailed "ffprobe timed out"
    Just (ExitFailure code, _, stderr) ->
      pure $ Left $ DurationExtractionFailed [i|ffprobe exited with code #{code}: #{decodeStderr stderr}|]
    Just (ExitSuccess, stdout, _) ->
      case Aeson.decode stdout of
        Nothing -> pure $ Left $ DurationExtractionFailed "Failed to parse ffprobe JSON output"
        Just obj -> case parseDuration obj of
          Nothing -> pure $ Left $ DurationExtractionFailed "No duration field in ffprobe output"
          Just d -> pure $ Right (round d)

--------------------------------------------------------------------------------
-- Helpers

-- | Parse the duration from ffprobe JSON output.
--
-- ffprobe returns: @{ "format": { "duration": "123.456" } }@
parseDuration :: Aeson.Value -> Maybe Double
parseDuration = Aeson.Types.parseMaybe parser
  where
    parser = Aeson.withObject "ffprobe" $ \obj -> do
      formatObj <- obj .: "format"
      durStr <- formatObj .: "duration"
      case reads (Text.unpack (durStr :: Text)) of
        [(d, "")] -> pure d
        _ -> fail "invalid duration"

-- | Decode stderr bytes to text, taking the last 500 characters for log messages.
decodeStderr :: LBS.ByteString -> Text
decodeStderr bs =
  let t = TE.decodeUtf8Lenient (LBS.toStrict bs)
   in Text.takeEnd 500 t
