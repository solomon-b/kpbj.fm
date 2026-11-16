{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

--------------------------------------------------------------------------------

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Effects.Database.Execute (execQuery)
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Job (JobM, runJob)
import Log qualified
import Options.Applicative

--------------------------------------------------------------------------------

-- | Number of days to retain tokens before purging.
tokenRetentionDays :: Int64
tokenRetentionDays = 90

--------------------------------------------------------------------------------

newtype Options = Options {optDryRun :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "dry-run" <> help "Print what would be deleted without deleting")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Clean up expired authentication tokens"))
  runJob "token-cleanup" $
    if optDryRun opts
      then runDryRun
      else runCleanup

--------------------------------------------------------------------------------

runCleanup :: JobM ()
runCleanup = do
  runDelete "expired email verification tokens" deleteExpiredVerificationTokens
  runDelete "expired password reset tokens" deleteExpiredPasswordResetTokens
  runDelete ("email verification tokens older than " <> show tokenRetentionDays <> " days") (deleteOldVerificationTokens tokenRetentionDays)
  runDelete ("password reset tokens older than " <> show tokenRetentionDays <> " days") (deleteOldPasswordResetTokens tokenRetentionDays)

runDryRun :: JobM ()
runDryRun = do
  Log.logInfo "Running in dry-run mode" ()
  countQuery "expired email verification tokens" countExpiredVerificationTokens
  countQuery "expired password reset tokens" countExpiredPasswordResetTokens
  countQuery ("email verification tokens older than " <> show tokenRetentionDays <> " days") (countOldVerificationTokens tokenRetentionDays)
  countQuery ("password reset tokens older than " <> show tokenRetentionDays <> " days") (countOldPasswordResetTokens tokenRetentionDays)

--------------------------------------------------------------------------------

runDelete :: String -> Hasql.Statement () () -> JobM ()
runDelete name statement =
  execQuery statement >>= \case
    Left err ->
      Log.logAttention "Cleanup error" $ Aeson.object ["category" .= name, "error" .= show err]
    Right () ->
      Log.logInfo "Cleaned up" $ Aeson.object ["category" .= name]

countQuery :: String -> Hasql.Statement () Int64 -> JobM ()
countQuery name statement =
  execQuery statement >>= \case
    Left err ->
      Log.logAttention "Count error" $ Aeson.object ["category" .= name, "error" .= show err]
    Right count ->
      Log.logInfo "Would delete" $ Aeson.object ["category" .= name, "count" .= count]

--------------------------------------------------------------------------------
-- Delete queries

deleteExpiredVerificationTokens :: Hasql.Statement () ()
deleteExpiredVerificationTokens =
  interp
    True
    [sql|
    DELETE FROM email_verification_tokens WHERE expires_at < NOW() AND status = 'pending'
  |]

deleteExpiredPasswordResetTokens :: Hasql.Statement () ()
deleteExpiredPasswordResetTokens =
  interp
    True
    [sql|
    DELETE FROM password_reset_tokens WHERE expires_at < NOW() AND status = 'pending'
  |]

deleteOldVerificationTokens :: Int64 -> Hasql.Statement () ()
deleteOldVerificationTokens days =
  interp
    True
    [sql|
    DELETE FROM email_verification_tokens WHERE created_at < NOW() - make_interval(days => #{days} :: int)
  |]

deleteOldPasswordResetTokens :: Int64 -> Hasql.Statement () ()
deleteOldPasswordResetTokens days =
  interp
    True
    [sql|
    DELETE FROM password_reset_tokens WHERE created_at < NOW() - make_interval(days => #{days} :: int)
  |]

--------------------------------------------------------------------------------
-- Count queries (for dry-run)

-- | Read the one row of a @SELECT COUNT(*)@.
--
-- A count with no @GROUP BY@ always returns one row, so the 0 is never used.
countRow :: Hasql.Statement () (Maybe (OneColumn Int64)) -> Hasql.Statement () Int64
countRow = fmap (maybe 0 getOneColumn)

countExpiredVerificationTokens :: Hasql.Statement () Int64
countExpiredVerificationTokens =
  countRow $
    interp
      True
      [sql|
      SELECT COUNT(*) FROM email_verification_tokens WHERE expires_at < NOW() AND status = 'pending'
    |]

countExpiredPasswordResetTokens :: Hasql.Statement () Int64
countExpiredPasswordResetTokens =
  countRow $
    interp
      True
      [sql|
      SELECT COUNT(*) FROM password_reset_tokens WHERE expires_at < NOW() AND status = 'pending'
    |]

countOldVerificationTokens :: Int64 -> Hasql.Statement () Int64
countOldVerificationTokens days =
  countRow $
    interp
      True
      [sql|
      SELECT COUNT(*) FROM email_verification_tokens WHERE created_at < NOW() - make_interval(days => #{days} :: int)
    |]

countOldPasswordResetTokens :: Int64 -> Hasql.Statement () Int64
countOldPasswordResetTokens days =
  countRow $
    interp
      True
      [sql|
      SELECT COUNT(*) FROM password_reset_tokens WHERE created_at < NOW() - make_interval(days => #{days} :: int)
    |]
