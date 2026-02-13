module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Data.Int (Int64)
import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

-- | Number of days to retain tokens before purging.
tokenRetentionDays :: Int64
tokenRetentionDays = 90

--------------------------------------------------------------------------------

newtype Options = Options { optDryRun :: Bool }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "Print what would be deleted without deleting")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Clean up expired authentication tokens"))
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mDbUrl of
    Nothing -> do
      hPutStrLn stderr "ERROR: DATABASE_URL environment variable is required"
      exitFailure
    Just dbUrl -> do
      connResult <- Connection.acquire [Setting.connection (Setting.Connection.string (Text.pack dbUrl))]
      case connResult of
        Left err -> do
          hPutStrLn stderr $ "ERROR: Failed to connect to database: " <> show err
          exitFailure
        Right conn ->
          bracket (pure conn) Connection.release $ \c ->
            if optDryRun opts
              then runDryRun c
              else runCleanup c

--------------------------------------------------------------------------------

runCleanup :: Connection.Connection -> IO ()
runCleanup conn = do
  runQuery conn "expired email verification tokens" deleteExpiredVerificationTokens
  runQuery conn "expired password reset tokens" deleteExpiredPasswordResetTokens
  runQuery conn ("email verification tokens older than " <> show tokenRetentionDays <> " days") (deleteOldVerificationTokens tokenRetentionDays)
  runQuery conn ("password reset tokens older than " <> show tokenRetentionDays <> " days") (deleteOldPasswordResetTokens tokenRetentionDays)

runDryRun :: Connection.Connection -> IO ()
runDryRun conn = do
  hPutStrLn stderr "[DRY RUN] Running in dry-run mode"
  countQuery conn "expired email verification tokens" countExpiredVerificationTokens
  countQuery conn "expired password reset tokens" countExpiredPasswordResetTokens
  countQuery conn ("email verification tokens older than " <> show tokenRetentionDays <> " days") (countOldVerificationTokens tokenRetentionDays)
  countQuery conn ("password reset tokens older than " <> show tokenRetentionDays <> " days") (countOldPasswordResetTokens tokenRetentionDays)

--------------------------------------------------------------------------------

runQuery :: Connection.Connection -> String -> Session.Session () -> IO ()
runQuery conn name session = do
  result <- try $ Session.run session conn
  case result of
    Left (e :: SomeException) ->
      hPutStrLn stderr $ "[token-cleanup] " <> name <> " cleanup failed: " <> show e
    Right (Left sessionErr) ->
      hPutStrLn stderr $ "[token-cleanup] " <> name <> " cleanup error: " <> show sessionErr
    Right (Right ()) ->
      hPutStrLn stderr $ "[token-cleanup] Cleaned up " <> name

countQuery :: Connection.Connection -> String -> Session.Session Int64 -> IO ()
countQuery conn name session = do
  result <- try $ Session.run session conn
  case result of
    Left (e :: SomeException) ->
      hPutStrLn stderr $ "[token-cleanup] " <> name <> " count failed: " <> show e
    Right (Left sessionErr) ->
      hPutStrLn stderr $ "[token-cleanup] " <> name <> " count error: " <> show sessionErr
    Right (Right count) ->
      hPutStrLn stderr $ "[token-cleanup] Would delete " <> show count <> " " <> name

--------------------------------------------------------------------------------
-- Delete queries

deleteExpiredVerificationTokens :: Session.Session ()
deleteExpiredVerificationTokens =
  Session.statement () $
    Statement.Statement
      "DELETE FROM email_verification_tokens WHERE expires_at < NOW() AND status = 'pending'"
      Encoders.noParams
      Decoders.noResult
      True

deleteExpiredPasswordResetTokens :: Session.Session ()
deleteExpiredPasswordResetTokens =
  Session.statement () $
    Statement.Statement
      "DELETE FROM password_reset_tokens WHERE expires_at < NOW() AND status = 'pending'"
      Encoders.noParams
      Decoders.noResult
      True

deleteOldVerificationTokens :: Int64 -> Session.Session ()
deleteOldVerificationTokens days =
  Session.statement days $
    Statement.Statement
      "DELETE FROM email_verification_tokens WHERE created_at < NOW() - make_interval(days => $1 :: int)"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      Decoders.noResult
      True

deleteOldPasswordResetTokens :: Int64 -> Session.Session ()
deleteOldPasswordResetTokens days =
  Session.statement days $
    Statement.Statement
      "DELETE FROM password_reset_tokens WHERE created_at < NOW() - make_interval(days => $1 :: int)"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      Decoders.noResult
      True

--------------------------------------------------------------------------------
-- Count queries (for dry-run)

countExpiredVerificationTokens :: Session.Session Int64
countExpiredVerificationTokens =
  Session.statement () $
    Statement.Statement
      "SELECT COUNT(*) FROM email_verification_tokens WHERE expires_at < NOW() AND status = 'pending'"
      Encoders.noParams
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
      True

countExpiredPasswordResetTokens :: Session.Session Int64
countExpiredPasswordResetTokens =
  Session.statement () $
    Statement.Statement
      "SELECT COUNT(*) FROM password_reset_tokens WHERE expires_at < NOW() AND status = 'pending'"
      Encoders.noParams
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
      True

countOldVerificationTokens :: Int64 -> Session.Session Int64
countOldVerificationTokens days =
  Session.statement days $
    Statement.Statement
      "SELECT COUNT(*) FROM email_verification_tokens WHERE created_at < NOW() - make_interval(days => $1 :: int)"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
      True

countOldPasswordResetTokens :: Int64 -> Session.Session Int64
countOldPasswordResetTokens days =
  Session.statement days $
    Statement.Statement
      "SELECT COUNT(*) FROM password_reset_tokens WHERE created_at < NOW() - make_interval(days => $1 :: int)"
      (Encoders.param (Encoders.nonNullable Encoders.int8))
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
      True
