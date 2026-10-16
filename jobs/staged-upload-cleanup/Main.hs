module Main (main) where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Config (Environment (..))
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (S3StorageConfig (..), StorageBackend (..))
import Effects.AWS (initAWSEnv)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Storage (deleteFile)
import Effects.Storage.Config (StorageConfigError (..), loadStorageConfig, selectStorageBackend)
import Effects.Storage.Staging (isStagingPath)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

newtype Options = Options {optDryRun :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "List the expired uploads and delete nothing")

-- | What the job did with one expired upload.
data Outcome
  = -- | The file and the row are deleted. In a dry run: the guard lets the path through.
    Cleaned
  | -- | The path is outside the staging prefix. Nothing is deleted.
    Refused
  | -- | The file or row delete failed. The row stays for the next run.
    Failed
  deriving (Eq)

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Delete expired staged uploads and their files"))
  dbUrl <- requireEnv "DATABASE_URL"
  envName <- requireEnv "APP_ENVIRONMENT"
  env <- maybe (die' ("APP_ENVIRONMENT must be Development, Staging, or Production, not " <> envName)) pure (parseEnvironment (Text.pack envName))
  s3ConfigResult <- loadStorageConfig
  backend <- either (die' . describeConfigError) pure (selectStorageBackend env s3ConfigResult)
  mAwsEnv <- case backend of
    S3Storage s3Config -> Just <$> initAWSEnv (s3Region s3Config) (s3EndpointUrl s3Config)
    LocalStorage _ -> pure Nothing
  connResult <- Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
  case connResult of
    Left err -> die' ("Failed to connect to database: " <> show err)
    Right conn -> do
      ok <-
        bracket (pure conn) Connection.release $ \c ->
          Log.withStdOutLogger $ \logger ->
            Log.runLogT "staged-upload-cleanup" logger Log.LogInfo $
              runJob opts backend mAwsEnv c
      unless ok exitFailure

--------------------------------------------------------------------------------

-- | Clean every expired upload. Gives False when any row was refused or failed.
runJob :: Options -> StorageBackend -> Maybe AWS.Env -> Connection.Connection -> Log.LogT IO Bool
runJob opts backend mAwsEnv conn = do
  mUploads <- runStatement conn "Failed to query expired uploads" [] StagedUploads.getExpiredUploads
  case mUploads of
    Nothing -> pure False
    Just uploads -> do
      outcomes <- forM uploads $ \upload ->
        if optDryRun opts
          then checkUpload upload
          else cleanUpload backend mAwsEnv conn upload
      let count outcome = length (filter (== outcome) outcomes)
          summary =
            Aeson.object
              [ "found" .= length uploads,
                "cleaned" .= count Cleaned,
                "refused" .= count Refused,
                "failed" .= count Failed
              ]
      Log.logInfo (if optDryRun opts then "Dry run finished, nothing was deleted" else "Cleanup finished") summary
      pure (count Refused == 0 && count Failed == 0)

-- | Dry run: report what the job would do with one upload.
checkUpload :: StagedUploads.Model -> Log.LogT IO Outcome
checkUpload upload
  | isStagingPath (StagedUploads.storagePath upload) =
      Cleaned <$ Log.logInfo "Would delete expired upload" (uploadFields upload [])
  | otherwise =
      Refused <$ Log.logAttention "Refused expired upload outside the staging prefix" (uploadFields upload [])

-- | Delete the file of one upload, then its row.
--
-- The row stays when the file delete fails, so the next run tries again.
cleanUpload :: StorageBackend -> Maybe AWS.Env -> Connection.Connection -> StagedUploads.Model -> Log.LogT IO Outcome
cleanUpload backend mAwsEnv conn upload
  | not (isStagingPath (StagedUploads.storagePath upload)) =
      Refused <$ Log.logAttention "Refused expired upload outside the staging prefix" (uploadFields upload [])
  | otherwise = do
      deleteResult <- deleteFile backend mAwsEnv (StagedUploads.storagePath upload)
      case deleteResult of
        Left err ->
          Failed <$ Log.logAttention "Failed to delete expired upload file" (uploadFields upload ["error" .= err])
        Right () -> do
          mDeleted <-
            runStatement
              conn
              "Failed to delete expired upload row"
              (uploadPairs upload)
              (StagedUploads.deleteById (StagedUploads.id upload))
          case mDeleted of
            Nothing -> pure Failed
            Just _ -> Cleaned <$ Log.logInfo "Deleted expired upload" (uploadFields upload [])

--------------------------------------------------------------------------------

-- | Run one statement. A failure is logged at attention level and gives Nothing.
runStatement ::
  (MonadIO m, Log.MonadLog m) =>
  Connection.Connection ->
  Text ->
  [Aeson.Types.Pair] ->
  Hasql.Statement () a ->
  m (Maybe a)
runStatement conn message pairs statement = do
  result <- liftIO $ try $ Session.run (Session.statement () statement) conn
  case result of
    Left (e :: SomeException) ->
      Nothing <$ Log.logAttention message (Aeson.object (pairs <> ["error" .= show e]))
    Right (Left sessionErr) ->
      Nothing <$ Log.logAttention message (Aeson.object (pairs <> ["error" .= show sessionErr]))
    Right (Right a) -> pure (Just a)

uploadPairs :: StagedUploads.Model -> [Aeson.Types.Pair]
uploadPairs upload =
  [ "id" .= StagedUploads.unId (StagedUploads.id upload),
    "storage_path" .= StagedUploads.storagePath upload
  ]

uploadFields :: StagedUploads.Model -> [Aeson.Types.Pair] -> Aeson.Value
uploadFields upload extra = Aeson.object (uploadPairs upload <> extra)

--------------------------------------------------------------------------------

-- | Parse APP_ENVIRONMENT. The names are the ones the web service accepts.
parseEnvironment :: Text -> Maybe Environment
parseEnvironment = \case
  "Development" -> Just Development
  "Staging" -> Just Staging
  "Production" -> Just Production
  _ -> Nothing

describeConfigError :: StorageConfigError -> String
describeConfigError = \case
  PartialS3ConfigError missing present ->
    "Partial S3 configuration. Missing: " <> show missing <> ". Present: " <> show present <> "."
  MissingS3ConfigError env ->
    "S3 configuration is required for " <> show env <> ". Set BUCKET_NAME and AWS_REGION."

requireEnv :: String -> IO String
requireEnv name = lookupEnv name >>= maybe (die' (name <> " environment variable is required")) pure

-- | Write an error line to stderr and exit with code 1.
die' :: String -> IO a
die' message = do
  hPutStrLn stderr ("ERROR: " <> message)
  exitFailure
