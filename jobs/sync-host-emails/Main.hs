module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database qualified
import Effects.Database.Execute (execQuery)
import GoogleGroups
import Job (runJob)
import Log qualified
import Options.Applicative
import Sync (SyncPlan (..), computeSyncPlan)
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

data Options = Options
  { optSaEmail :: String,
    optSaPrivateKey :: String,
    optDelegatedUser :: String,
    optGroupEmail :: String,
    optDryRun :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "sa-email" <> metavar "EMAIL" <> help "Service account email")
    <*> strOption (long "sa-private-key" <> metavar "KEY" <> help "RSA private key (PEM format)")
    <*> strOption (long "delegated-user" <> metavar "EMAIL" <> help "Google Workspace admin email to impersonate")
    <*> strOption (long "group-email" <> metavar "EMAIL" <> help "Google Group email to list members of")
    <*> switch (long "dry-run" <> help "Dry run mode (no side effects)")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Sync host email membership"))
  runJob "sync-host-emails" $ do
    let ggConfig =
          GoogleGroupsConfig
            { ggSaEmail = Text.pack (optSaEmail opts),
              ggSaKey = Text.pack (optSaPrivateKey opts),
              ggDelegatedUser = Text.pack (optDelegatedUser opts),
              ggGroupEmail = Text.pack (optGroupEmail opts)
            }
        groupEmail = Text.pack (optGroupEmail opts)
        dryRun = optDryRun opts

    when dryRun $
      Log.logInfo "Running in dry-run mode" ()

    -- Acquire a single OAuth2 token for all API calls
    Log.logInfo "Acquiring Google API token" ()
    token <- case' =<< liftIO (acquireToken ggConfig)

    -- Fetch Google Group members
    Log.logInfo "Fetching Google Group members" ()
    groupMembers <- case' =<< liftIO (listGroupMembers token groupEmail)
    Log.logInfo "Found Google Group members" $ Aeson.object ["count" .= length groupMembers]

    -- Fetch host emails from the database
    Log.logInfo "Fetching host emails from database" ()
    emails <-
      execQuery Database.fetchHostEmails >>= \case
        Left err -> do
          Log.logAttention "Database query failed" $ Aeson.object ["error" .= show err]
          liftIO exitFailure
        Right es -> do
          Log.logInfo "Found hosts in database" $ Aeson.object ["count" .= length es]
          pure es

    -- Compute the reconciliation plan (case-insensitive, manager-protected)
    let plan = computeSyncPlan groupMembers emails

    Log.logInfo "Computed diff" $
      Aeson.object
        [ "to_remove" .= length (spToRemove plan),
          "to_add" .= length (spToAdd plan),
          "protected" .= length (spProtected plan)
        ]

    -- Perform the sync
    if dryRun
      then liftIO $ do
        printSection "WOULD REMOVE FROM GOOGLE GROUP" (spToRemove plan)
        printSection "WOULD ADD TO GOOGLE GROUP" (spToAdd plan)
        printSection "PROTECTED (skipped)" (spProtected plan)
      else do
        -- Remove non-host MEMBERs (managers/owners are excluded by the plan)
        mapM_ (removeMember token groupEmail) (spToRemove plan)

        -- Add hosts missing from the group
        mapM_ (addMember token groupEmail) (spToAdd plan)

        Log.logInfo "Sync complete" ()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Print a titled section of emails to stdout for dry-run output.
printSection :: Text.Text -> [Text.Text] -> IO ()
printSection title items = do
  Text.putStrLn "========================================"
  Text.putStrLn title
  Text.putStrLn "========================================"
  if null items
    then Text.putStrLn "(none)"
    else mapM_ Text.putStrLn items
  Text.putStrLn ""

-- | Unwrap a GoogleGroupsError or die.
case' :: (MonadIO m, Log.MonadLog m) => Either GoogleGroupsError a -> m a
case' (Right a) = pure a
case' (Left err) = do
  Log.logAttention "Google API error" $ Aeson.object ["error" .= Text.unpack (displayError err)]
  liftIO exitFailure

removeMember :: (MonadIO m, Log.MonadLog m) => AccessToken -> Text.Text -> Text.Text -> m ()
removeMember token groupEmail email = do
  Log.logInfo "Removing member" $ Aeson.object ["email" .= email]
  result <- liftIO $ removeGroupMember token groupEmail email
  case result of
    Right () -> Log.logInfo "Removed" $ Aeson.object ["email" .= email]
    Left err -> Log.logAttention "Failed to remove member" $ Aeson.object ["email" .= email, "error" .= Text.unpack (displayError err)]

addMember :: (MonadIO m, Log.MonadLog m) => AccessToken -> Text.Text -> Text.Text -> m ()
addMember token groupEmail email = do
  Log.logInfo "Adding member" $ Aeson.object ["email" .= email]
  result <- liftIO $ addGroupMember token groupEmail email
  case result of
    Right () -> Log.logInfo "Added" $ Aeson.object ["email" .= email]
    Left err -> Log.logAttention "Failed to add member" $ Aeson.object ["email" .= email, "error" .= Text.unpack (displayError err)]
