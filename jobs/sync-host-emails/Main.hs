module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GoogleGroups
import Options.Applicative
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
  let config =
        GoogleGroupsConfig
          { ggSaEmail = Text.pack (optSaEmail opts),
            ggSaKey = Text.pack (optSaPrivateKey opts),
            ggDelegatedUser = Text.pack (optDelegatedUser opts),
            ggGroupEmail = Text.pack (optGroupEmail opts)
          }
  when (optDryRun opts) $
    hPutStrLn stderr "[DRY RUN] Running in dry-run mode"
  result <- listGroupMembers config
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> Text.unpack (displayError err)
      exitFailure
    Right members ->
      mapM_ printMember members

printMember :: GroupMember -> IO ()
printMember GroupMember {..} =
  Text.putStrLn $ gmEmail <> "\t" <> roleText gmRole <> "\t" <> gmType <> "\t" <> gmStatus

roleText :: GroupMemberRole -> Text.Text
roleText = \case
  RoleOwner -> "OWNER"
  RoleManager -> "MANAGER"
  RoleMember -> "MEMBER"
  RoleUnknown t -> t
