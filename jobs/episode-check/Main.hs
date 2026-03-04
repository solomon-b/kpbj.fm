module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, bracket, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Functor ((<&>))
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LT
import Data.Time (TimeOfDay, defaultTimeLocale, formatTime)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Email.Config (SmtpConfig, loadSmtpConfig)
import Effects.Email.Send qualified as Email
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Session qualified as Session
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import OrphanInstances.DayOfWeek ()
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

data Options = Options
  { optDays :: [Int32],
    optDryRun :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> ( many
            ( option
                auto
                ( long "days"
                    <> metavar "N"
                    <> help "Days ahead to check (repeatable, default: 5 3 1)"
                )
            )
            <&> \case
              [] -> [5, 3, 1]
              ds -> ds
        )
    <*> switch
      ( long "dry-run"
          <> help "Print what would be sent without sending emails"
      )

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Check for shows with upcoming episodes missing audio"))
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
            Log.withStdOutLogger $ \logger ->
              Log.runLogT "episode-check" logger Log.LogInfo $
                if optDryRun opts
                  then mapM_ (runDryRun c) (optDays opts)
                  else runNotify c (optDays opts)

--------------------------------------------------------------------------------
-- Query

queryHosts :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> Int32 -> m (Maybe [ShowSchedule.HostMissingEpisode])
queryHosts conn days = do
  result <- liftIO $ try $ Session.run (Session.statement () (ShowSchedule.getHostsMissingEpisodesOnDay days)) conn
  case result of
    Left (e :: SomeException) -> do
      Log.logAttention "Query failed" $ Aeson.object ["error" .= show e]
      liftIO exitFailure
    Right (Left sessionErr) -> do
      Log.logAttention "Session error" $ Aeson.object ["error" .= show sessionErr]
      liftIO exitFailure
    Right (Right []) -> do
      Log.logInfo "No hosts to notify" $ Aeson.object ["days" .= days]
      pure Nothing
    Right (Right hosts) ->
      pure (Just hosts)

--------------------------------------------------------------------------------
-- Dry run (log only)

runDryRun :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> Int32 -> m ()
runDryRun conn days = do
  Log.logInfo "DRY RUN — no emails will be sent" ()
  queryHosts conn days >>= \case
    Nothing -> pure ()
    Just hosts -> do
      Log.logInfo "Would send notifications" $ Aeson.object ["count" .= length hosts]
      mapM_ printHostNotification hosts

printHostNotification :: (Log.MonadLog m) => ShowSchedule.HostMissingEpisode -> m ()
printHostNotification ep =
  Log.logInfo "Would notify host" $
    Aeson.object
      [ "to" .= ShowSchedule.hmeHostEmail ep,
        "show" .= ShowSchedule.hmeShowTitle ep,
        "date" .= formatTime defaultTimeLocale "%Y-%m-%d" (ShowSchedule.hmeShowDate ep),
        "day" .= display (ShowSchedule.hmeDayOfWeek ep),
        "start" .= formatTime defaultTimeLocale "%l:%M %p" (ShowSchedule.hmeStartTime ep),
        "end" .= formatTime defaultTimeLocale "%l:%M %p" (ShowSchedule.hmeEndTime ep)
      ]

--------------------------------------------------------------------------------
-- Notify (send emails)

runNotify :: (MonadIO m, Log.MonadLog m) => Connection.Connection -> [Int32] -> m ()
runNotify conn daysList = do
  mSmtpConfig <- liftIO loadSmtpConfig
  case mSmtpConfig of
    Nothing -> do
      Log.logAttention "SMTP not configured. Set APP_SMTP_* env vars or use --dry-run." ()
      liftIO exitFailure
    Just smtpConfig -> do
      totalFailures <- sum <$> mapM (runNotifyForDay smtpConfig conn) daysList
      if totalFailures > 0
        then do
          Log.logAttention "Some notifications failed" $ Aeson.object ["failed" .= totalFailures]
          liftIO exitFailure
        else Log.logInfo "All notifications sent successfully" ()

runNotifyForDay :: (MonadIO m, Log.MonadLog m) => SmtpConfig -> Connection.Connection -> Int32 -> m Int
runNotifyForDay smtpConfig conn days =
  queryHosts conn days >>= \case
    Nothing -> pure 0
    Just hosts -> do
      Log.logInfo "Sending notifications" $ Aeson.object ["days" .= days, "count" .= length hosts]
      results <- mapM (sendHostNotification smtpConfig days) hosts
      pure $ length (filter not results)

sendHostNotification :: (MonadIO m, Log.MonadLog m) => SmtpConfig -> Int32 -> ShowSchedule.HostMissingEpisode -> m Bool
sendHostNotification config days ep = do
  let email = buildEpisodeReminderEmail days ep
  result <- liftIO $ try $ Email.send config email
  case result of
    Left (e :: SomeException) -> do
      Log.logAttention "Failed to send notification" $
        Aeson.object
          [ "to" .= ShowSchedule.hmeHostEmail ep,
            "error" .= show e
          ]
      pure False
    Right () -> do
      Log.logInfo "Sent reminder" $
        Aeson.object
          [ "to" .= ShowSchedule.hmeHostEmail ep,
            "show" .= ShowSchedule.hmeShowTitle ep
          ]
      pure True

--------------------------------------------------------------------------------
-- Email template

buildEpisodeReminderEmail :: Int32 -> ShowSchedule.HostMissingEpisode -> Email.Email
buildEpisodeReminderEmail days ep =
  let hostName = ShowSchedule.hmeHostDisplayName ep
      showTitle = ShowSchedule.hmeShowTitle ep
      dateStr = Text.pack $ formatTime defaultTimeLocale "%B %e, %Y" (ShowSchedule.hmeShowDate ep)
      dayName = display (ShowSchedule.hmeDayOfWeek ep)
      startStr = formatTimeOfDay (ShowSchedule.hmeStartTime ep)
      endStr = formatTimeOfDay (ShowSchedule.hmeEndTime ep)
      (urgencyTag, timeLeft, greeting) = case days of
        1 -> ("URGENT: " :: Text, "TOMORROW" :: Text, [i|This is a final reminder — your show "#{showTitle}" airs TOMORROW,|] :: Text)
        3 -> ("Reminder: ", "in 3 days", [i|Just a reminder — your show "#{showTitle}" is coming up in 3 days,|])
        _ -> ("Heads up: ", [i|in #{days} days|], [i|Your show "#{showTitle}" is scheduled to air in #{days} days,|])
   in Email.Email
        { Email.emailTo = ShowSchedule.hmeHostEmail ep,
          Email.emailSubject = urgencyTag <> showTitle <> " - KPBJ 95.9FM",
          Email.emailBody =
            LT.fromStrict
              [i|
================================================================
                  KPBJ 95.9FM COMMUNITY RADIO
================================================================

Hey #{hostName},

#{greeting}
on #{dateStr} (#{dayName} #{startStr} - #{endStr} PT), and we
don't have an episode uploaded yet.

Your episode airs #{timeLeft}. Please upload as soon as you can.

UPLOAD YOUR EPISODE
-------------------
Head to your host dashboard to upload:

https://stream.kpbj.fm/dashboard/host

NEED HELP?
----------
Questions? Reach out to station staff.

Discord: https://discord.gg/Pt5pnK7X
Email: contact@kpbj.fm

--
The KPBJ Team
https://kpbj.fm

================================================================
|],
          Email.emailLabel = "episode-reminder"
        }

-- | Format a TimeOfDay as "8:00 PM"
formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay = Text.pack . formatTime defaultTimeLocale "%l:%M %p"
