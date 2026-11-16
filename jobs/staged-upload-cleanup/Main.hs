module Main (main) where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Config (Environment (..))
import Control.Monad (forM, unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (S3StorageConfig (..), StorageBackend (..))
import Effects.AWS (initAWSEnv)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Storage (deleteFile)
import Effects.Storage.Config (StorageConfigError (..), loadStorageConfig, selectStorageBackend)
import Effects.Storage.Staging (isStagingPath)
import Job (JobM, exitWithError, requireEnv, runJob)
import Log qualified
import Options.Applicative
import System.Exit (exitFailure)

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
  envName <- requireEnv "APP_ENVIRONMENT"
  env <- maybe (exitWithError ("APP_ENVIRONMENT must be Development, Staging, or Production, not " <> envName)) pure (parseEnvironment (Text.pack envName))
  s3ConfigResult <- loadStorageConfig
  backend <- either (exitWithError . describeConfigError) pure (selectStorageBackend env s3ConfigResult)
  mAwsEnv <- case backend of
    S3Storage s3Config -> Just <$> initAWSEnv (s3Region s3Config) (s3EndpointUrl s3Config)
    LocalStorage _ -> pure Nothing
  ok <- runJob "staged-upload-cleanup" (cleanExpiredUploads opts backend mAwsEnv)
  unless ok exitFailure

--------------------------------------------------------------------------------

-- | Clean every expired upload. Gives False when any row was refused or failed.
cleanExpiredUploads :: Options -> StorageBackend -> Maybe AWS.Env -> JobM Bool
cleanExpiredUploads opts backend mAwsEnv =
  execQuery StagedUploads.getExpiredUploads >>= \case
    Left err ->
      False <$ Log.logAttention "Failed to query expired uploads" (Aeson.object ["error" .= show err])
    Right uploads -> do
      outcomes <- forM uploads $ \upload ->
        if optDryRun opts
          then checkUpload upload
          else cleanUpload backend mAwsEnv upload
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
checkUpload :: StagedUploads.Model -> JobM Outcome
checkUpload upload
  | isStagingPath (StagedUploads.storagePath upload) =
      Cleaned <$ Log.logInfo "Would delete expired upload" (uploadFields upload [])
  | otherwise =
      Refused <$ Log.logAttention "Refused expired upload outside the staging prefix" (uploadFields upload [])

-- | Delete the file of one upload, then its row.
--
-- The row stays when the file delete fails, so the next run tries again.
cleanUpload :: StorageBackend -> Maybe AWS.Env -> StagedUploads.Model -> JobM Outcome
cleanUpload backend mAwsEnv upload
  | not (isStagingPath (StagedUploads.storagePath upload)) =
      Refused <$ Log.logAttention "Refused expired upload outside the staging prefix" (uploadFields upload [])
  | otherwise = do
      deleteResult <- deleteFile backend mAwsEnv (StagedUploads.storagePath upload)
      case deleteResult of
        Left err ->
          Failed <$ Log.logAttention "Failed to delete expired upload file" (uploadFields upload ["error" .= err])
        Right () ->
          execQuery (StagedUploads.deleteById (StagedUploads.id upload)) >>= \case
            Left err ->
              Failed <$ Log.logAttention "Failed to delete expired upload row" (uploadFields upload ["error" .= show err])
            Right _ -> Cleaned <$ Log.logInfo "Deleted expired upload" (uploadFields upload [])

--------------------------------------------------------------------------------

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
