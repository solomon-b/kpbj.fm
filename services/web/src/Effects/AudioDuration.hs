{-# LANGUAGE ScopedTypeVariables #-}

-- | Read the length of an audio file with @ffprobe@.
--
-- The break window divides two minutes between a station ID and the spots that
-- fit, so it needs each file's length. Reading it here rather than in the
-- browser keeps the number off the client and works for files an @\<audio\>@
-- element cannot decode.
module Effects.AudioDuration
  ( probeDurationSeconds,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Int (Int64)
import Data.Text qualified as Text
import Log qualified
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcessStdout)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- | The length of an audio file in whole seconds, rounded up.
--
-- Rounds up so the break budget never plans for less time than a spot takes.
--
-- Returns 'Nothing' when @ffprobe@ is missing, fails, or reports a length that
-- is not a positive number. A file that gets no answer here is one the stream
-- cannot play either.
probeDurationSeconds :: (MonadIO m, Log.MonadLog m) => FilePath -> m (Maybe Int64)
probeDurationSeconds filePath = do
  result <- liftIO $ try $ readProcessStdout (proc "ffprobe" args)
  case result of
    Left (err :: SomeException) -> do
      Log.logAttention "ffprobe could not be run" (filePath, Text.pack $ show err)
      pure Nothing
    Right (ExitFailure code, _) -> do
      Log.logAttention "ffprobe exited with a failure" (filePath, code)
      pure Nothing
    Right (ExitSuccess, out) ->
      -- readMaybe skips the trailing newline ffprobe writes.
      case readMaybe (LBS8.unpack out) :: Maybe Double of
        Just seconds | seconds > 0 -> pure $ Just (ceiling seconds)
        _ -> do
          Log.logAttention "ffprobe reported no usable duration" (filePath, LBS8.unpack out)
          pure Nothing
  where
    -- -show_entries with -of csv=p=0 prints the duration alone, so there is
    -- nothing to parse around it.
    args =
      [ "-v",
        "error",
        "-show_entries",
        "format=duration",
        "-of",
        "csv=p=0",
        filePath
      ]
