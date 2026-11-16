module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule (getCalendarSeriesRows)
import Event (RemoteEvent (..), decodeRemoteEvent, encodeEvent, seriesToEvent)
import GoogleCalendar
import Job (runJobWith)
import Log qualified
import Options.Applicative hiding (action)
import Rrule (printRule)
import Series (clipOverlaps, seriesFromRow)
import Sync (Action (..), describeAction, handEdits, planSync, runActions)
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

data Options = Options
  { optSaEmail :: String,
    optSaPrivateKey :: String,
    optCalendarId :: String,
    optBaseUrl :: String,
    optDryRun :: Bool,
    optDump :: Bool,
    optVerbose :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "sa-email" <> metavar "EMAIL" <> help "Service account email")
    <*> strOption (long "sa-private-key" <> metavar "KEY" <> help "RSA private key (PEM format)")
    <*> strOption (long "calendar-id" <> metavar "ID" <> help "Google Calendar ID to sync")
    <*> strOption (long "base-url" <> metavar "URL" <> help "Website base URL for show links, with no slash at the end")
    <*> switch (long "dry-run" <> help "Dry run mode (no side effects)")
    <*> switch (long "dump" <> help "Print each raw calendar event as one line of JSON, then exit")
    <*> switch (long "verbose" <> help "Log at trace level, with one line for each action of the plan")

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Sync the broadcast schedule to a Google Calendar"))
  -- The timer runs the job at info level, where the log has only the counts
  -- of the plan. The trace level adds one line for each action.
  let maxLevel = if optVerbose opts then Log.LogTrace else Log.LogInfo
  runJobWith maxLevel "sync-schedule-calendar" $ do
    let config =
          CalendarConfig
            { ccSaEmail = Text.pack (optSaEmail opts),
              ccSaKey = Text.pack (optSaPrivateKey opts),
              ccCalendarId = Text.pack (optCalendarId opts)
            }
        calendarId = ccCalendarId config
        dryRun = optDryRun opts

    when dryRun $
      Log.logInfo "Running in dry-run mode" ()

    Log.logInfo "Acquiring Google API token" ()
    token <- orDie =<< liftIO (acquireToken config)

    Log.logInfo "Fetching calendar events" ()
    rawEvents <- orDie =<< liftIO (listEvents token calendarId)
    Log.logInfo "Found calendar events" $ Aeson.object ["count" .= length rawEvents]

    if optDump opts
      then liftIO $ mapM_ (LBS.putStrLn . Aeson.encode) rawEvents
      else do
        -- An event that does not decode is not in the plan. The run still
        -- does the remaining work, and then fails.
        let (undecoded, remote) = partitionEithers (map decodeRemoteEvent rawEvents)
        forM_ undecoded $ \err ->
          Log.logAttention "Failed to read a calendar event" $ Aeson.object ["error" .= err]

        Log.logInfo "Fetching schedule from database" ()
        rows <-
          execQuery getCalendarSeriesRows >>= \case
            Left err -> do
              Log.logAttention "Database query failed" $ Aeson.object ["error" .= show err]
              liftIO exitFailure
            Right rows -> pure rows
        Log.logInfo "Found validity windows in database" $ Aeson.object ["count" .= length rows]

        -- The station date. The UTC date is one day later for the last hours
        -- of each station day.
        today <- liftIO (pacificDay <$> getCurrentTime)
        let series = clipOverlaps (concatMap seriesFromRow rows)
            plan = planSync today (seriesToEvent (Text.pack (optBaseUrl opts))) series remote

        -- At info level the log has the counts only. The line for each
        -- action is at trace level. A failed action logs its own line.
        Log.logInfo "Computed plan" $
          Aeson.object
            [ "insert" .= length [() | Insert {} <- plan],
              "update" .= length [() | Update {} <- plan],
              "end" .= length [() | SetLastDay {} <- plan],
              "delete" .= length [() | Delete {} <- plan]
            ]

        -- Nobody must edit the calendar by hand. The job does not repair one
        -- airing. This line reaches the batch-job alert, and a person
        -- corrects the airing in Google Calendar.
        forM_ (handEdits today remote) $ \case
          RemoteException {..} ->
            Log.logAttention "A person changed one airing in the calendar. Correct it by hand." $
              Aeson.object
                [ "event_id" .= rxId,
                  "series_event_id" .= rxParentId,
                  "air_date" .= rxOriginalDay,
                  "change" .= (if rxCancelled then "deleted" else "moved or edited" :: Text)
                ]
          RemoteSeries {} -> pure ()

        failures <-
          if dryRun
            then [] <$ forM_ plan (\action -> Log.logTrace_ ("Would " <> describeAction action))
            else runActions (execute token calendarId) plan

        unless (null undecoded && null failures) $ liftIO exitFailure
        unless dryRun $ Log.logInfo "Sync complete" ()

--------------------------------------------------------------------------------
-- Actions

-- | Apply one action to the calendar. A failure is logged here and returned.
execute :: (MonadIO m, Log.MonadLog m) => AccessToken -> Text -> Action -> m (Either CalendarError ())
execute token calendarId action = do
  Log.logTrace_ ("Applying: " <> describeAction action)
  result <- liftIO $ case action of
    Insert event -> insertEvent token calendarId (encodeEvent event)
    Update eventId event -> updateEvent token calendarId eventId (encodeEvent event)
    SetLastDay eventId rule -> patchEvent token calendarId eventId (Aeson.object ["recurrence" .= [printRule rule]])
    Delete eventId -> deleteEvent token calendarId eventId
  case result of
    Right () -> pure ()
    Left err ->
      Log.logAttention "Failed to apply action" $
        Aeson.object ["action" .= describeAction action, "error" .= displayError err]
  pure result

--------------------------------------------------------------------------------
-- Helpers

-- | Unwrap a result, or log the error and exit with a failure status.
orDie :: (MonadIO m, Log.MonadLog m) => Either CalendarError a -> m a
orDie (Right a) = pure a
orDie (Left err) = do
  Log.logAttention "Google API error" $ Aeson.object ["error" .= displayError err]
  liftIO exitFailure
