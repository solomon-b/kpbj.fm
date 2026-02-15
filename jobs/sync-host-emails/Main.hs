module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database qualified
import GoogleGroups
import Hasql.Session qualified as Session
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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
    hPutStrLn stderr "[DRY RUN] Running in dry-run mode"

  -- Acquire a single OAuth2 token for all API calls
  hPutStrLn stderr "[sync-host-emails] Acquiring Google API token..."
  token <- case' =<< acquireToken ggConfig

  -- Fetch Google Group members
  hPutStrLn stderr "[sync-host-emails] Fetching Google Group members..."
  groupMembers <- case' =<< listGroupMembers token groupEmail
  hPutStrLn stderr $ "[sync-host-emails] Found " <> show (length groupMembers) <> " Google Group members"

  -- Fetch host emails from the database
  hPutStrLn stderr "[sync-host-emails] Fetching host emails from database..."
  mDbUrl <- lookupEnv "DATABASE_URL"
  dbUrl <- case mDbUrl of
    Nothing -> do
      hPutStrLn stderr "ERROR: DATABASE_URL environment variable is required"
      exitFailure
    Just url -> pure (Text.pack url)

  hostEmails <-
    Database.withConnection dbUrl $ \conn -> do
      result <- Session.run Database.fetchHostEmails conn
      case result of
        Left sessionErr -> do
          hPutStrLn stderr $ "[sync-host-emails] ERROR: Database query failed: " <> show sessionErr
          exitFailure
        Right emails -> do
          hPutStrLn stderr $ "[sync-host-emails] Found " <> show (length emails) <> " hosts in database"
          pure emails

  emails <- case hostEmails of
    Left err -> do
      hPutStrLn stderr $ "[sync-host-emails] ERROR: " <> err
      exitFailure
    Right es -> pure es

  -- Compute set differences (case-insensitive)
  let groupEmailSet = Set.fromList $ map (Text.toLower . gmEmail) groupMembers
      hostEmailSet = Set.fromList $ map Text.toLower emails
      toRemove = Set.difference groupEmailSet hostEmailSet
      toAdd = Set.difference hostEmailSet groupEmailSet

  -- Report the diff
  hPutStrLn stderr ""
  hPutStrLn stderr $ "  To remove from group: " <> show (Set.size toRemove)
  hPutStrLn stderr $ "  To add to group:      " <> show (Set.size toAdd)
  hPutStrLn stderr ""

  -- Perform the sync
  if dryRun
    then do
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

      hPutStrLn stderr "[sync-host-emails] Sync complete."

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Unwrap a GoogleGroupsError or die.
case' :: Either GoogleGroupsError a -> IO a
case' (Right a) = pure a
case' (Left err) = do
  hPutStrLn stderr $ "[sync-host-emails] ERROR: " <> Text.unpack (displayError err)
  exitFailure

removeMember :: AccessToken -> Text.Text -> Text.Text -> IO ()
removeMember token groupEmail email = do
  hPutStrLn stderr $ "[sync-host-emails] Removing: " <> Text.unpack email
  result <- removeGroupMember token groupEmail email
  case result of
    Right () -> hPutStrLn stderr $ "[sync-host-emails]   OK"
    Left err -> hPutStrLn stderr $ "[sync-host-emails]   FAILED: " <> Text.unpack (displayError err)

addMember :: AccessToken -> Text.Text -> Text.Text -> IO ()
addMember token groupEmail email = do
  hPutStrLn stderr $ "[sync-host-emails] Adding: " <> Text.unpack email
  result <- addGroupMember token groupEmail email
  case result of
    Right () -> hPutStrLn stderr $ "[sync-host-emails]   OK"
    Left err -> hPutStrLn stderr $ "[sync-host-emails]   FAILED: " <> Text.unpack (displayError err)
