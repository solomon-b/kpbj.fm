module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database qualified
import GoogleGroups
import Hasql.Session qualified as Session
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Options.Applicative
import System.Environment (lookupEnv)
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
  Log.withStdOutLogger $ \logger ->
    Log.runLogT "sync-host-emails" logger Log.LogInfo $ do
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
      mDbUrl <- liftIO $ lookupEnv "DATABASE_URL"
      dbUrl <- case mDbUrl of
        Nothing -> do
          Log.logAttention "DATABASE_URL environment variable is required" ()
          liftIO exitFailure
        Just url -> pure (Text.pack url)

      hostEmails <-
        liftIO $
          Database.withConnection dbUrl $ \conn ->
            Session.run Database.fetchHostEmails conn

      emails <- case hostEmails of
        Left err -> do
          Log.logAttention "Database connection error" $ Aeson.object ["error" .= err]
          liftIO exitFailure
        Right (Left sessionErr) -> do
          Log.logAttention "Database query failed" $ Aeson.object ["error" .= show sessionErr]
          liftIO exitFailure
        Right (Right es) -> do
          Log.logInfo "Found hosts in database" $ Aeson.object ["count" .= length es]
          pure es

      -- Compute set differences (case-insensitive)
      let groupEmailSet = Set.fromList $ map (Text.toLower . gmEmail) groupMembers
          hostEmailSet = Set.fromList $ map Text.toLower emails
          toRemove = Set.difference groupEmailSet hostEmailSet
          toAdd = Set.difference hostEmailSet groupEmailSet

      Log.logInfo "Computed diff" $
        Aeson.object
          [ "to_remove" .= Set.size toRemove,
            "to_add" .= Set.size toAdd
          ]

      -- Perform the sync
      if dryRun
        then liftIO $ do
          Text.putStrLn "========================================"
          Text.putStrLn "WOULD REMOVE FROM GOOGLE GROUP"
          Text.putStrLn "========================================"
          if Set.null toRemove
            then Text.putStrLn "(none)"
            else mapM_ Text.putStrLn (sort $ Set.toList toRemove)

          Text.putStrLn ""
          Text.putStrLn "========================================"
          Text.putStrLn "WOULD ADD TO GOOGLE GROUP"
          Text.putStrLn "========================================"
          if Set.null toAdd
            then Text.putStrLn "(none)"
            else mapM_ Text.putStrLn (sort $ Set.toList toAdd)
        else do
          -- Remove members not in DB
          mapM_ (removeMember token groupEmail) (sort $ Set.toList toRemove)

          -- Add hosts missing from group
          mapM_ (addMember token groupEmail) (sort $ Set.toList toAdd)

          Log.logInfo "Sync complete" ()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

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
