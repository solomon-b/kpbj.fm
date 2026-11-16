-- | The monad that the scheduled jobs in @jobs/@ run in.
--
-- A job holds one database connection for its whole run. 'JobM' has a
-- 'MonadDB' instance, so a job runs statements with @execQuery@, as the web
-- service does.
module Job
  ( -- * Job monad
    JobM,
    runJob,
    runJobWith,

    -- * Setup failures
    exitWithError,
    requireEnv,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (SomeAsyncException, SomeException, finally, fromException)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Class (MonadDB (..))
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Pool (UsageError (..))
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Transaction
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Job monad

-- | A scheduled job. It reads one database connection and writes log lines.
newtype JobM a = JobM (Connection -> Log.LoggerEnv -> IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadUnliftIO, Log.MonadLog)
    via ReaderT Connection (Log.LogT IO)

-- | Every statement of a job runs on its one connection.
instance MonadDB JobM where
  runDB :: Session a -> JobM (Either UsageError a)
  runDB session =
    JobM $ \conn _ -> first SessionUsageError <$> Session.run session conn

  runDBTransaction :: Transaction.Transaction a -> JobM (Either UsageError a)
  runDBTransaction =
    runDB . Transaction.transaction Transaction.Serializable Transaction.Write

-- | Run a job. The log has the lines at info level and above.
--
-- See 'runJobWith' for the setup and the failures.
runJob :: Text -> JobM a -> IO a
runJob = runJobWith Log.LogInfo

-- | Run a job with a given maximum log level.
--
-- A job with a verbose flag passes 'Log.LogTrace' here, and the log then has
-- the trace lines too.
--
-- The name is the component of each log line. The connection string comes
-- from @DATABASE_URL@. The connection closes when the job ends.
--
-- A missing @DATABASE_URL@ or a failed connection stops the program with
-- 'exitWithError'. An exception that escapes the job is logged at attention
-- level, and the program exits with code 1.
runJobWith :: Log.LogLevel -> Text -> JobM a -> IO a
runJobWith maxLevel name job = do
  dbUrl <- requireEnv "DATABASE_URL"
  Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))] >>= \case
    Left err -> exitWithError ("Failed to connect to database: " <> show err)
    Right conn ->
      flip finally (Connection.release conn) $
        Log.withStdOutLogger $ \logger ->
          Log.runLogT name logger maxLevel $
            runReaderT (coerce (logEscapedException job)) conn

-- | Log an exception that escapes the job, then exit with code 1.
--
-- An 'ExitCode' is how a job stops itself, so it passes through. An
-- asynchronous exception comes from outside the job, so it passes through
-- too.
logEscapedException :: JobM a -> JobM a
logEscapedException job =
  job `catch` \(e :: SomeException) ->
    case (fromException e :: Maybe ExitCode, fromException e :: Maybe SomeAsyncException) of
      (Nothing, Nothing) -> do
        Log.logAttention "Job failed with an exception" $ Aeson.object ["error" .= show e]
        liftIO exitFailure
      _ -> throwM e

--------------------------------------------------------------------------------
-- Setup failures

-- | Write an @ERROR:@ line to stderr and exit with code 1.
--
-- Use it for a failure before the logger exists.
exitWithError :: String -> IO a
exitWithError message = do
  hPutStrLn stderr ("ERROR: " <> message)
  exitFailure

-- | Read an environment variable. Stop with 'exitWithError' if it is not set.
requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= maybe (exitWithError (name <> " environment variable is required")) pure
