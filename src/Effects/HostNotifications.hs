{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Host notification effects.
--
-- Provides functionality for notifying hosts when they are assigned to shows.
module Effects.HostNotifications
  ( -- * Email Notifications
    sendHostAssignmentNotifications,
    formatTimeslotDescription,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks, showsLinks)
import API.Types
import App.Monad (AppM)
import App.Smtp (SmtpConfig (..))
import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (DayOfWeek (..), TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.MailSender qualified as MailSender
import Log qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Send host assignment notification emails to a list of user IDs.
--
-- Fetches each user's email and display name, then sends the notification.
-- This is fire-and-forget (async) to avoid blocking the request handler.
sendHostAssignmentNotifications ::
  -- | Show information
  Shows.Model ->
  -- | Human-readable timeslot (e.g., "Fridays 8:00 PM - 10:00 PM PT")
  Maybe Text ->
  -- | User IDs to notify
  [User.Id] ->
  AppM ()
sendHostAssignmentNotifications showModel mTimeslot hostIds = do
  mSmtpConfig <- asks Has.getter
  case mSmtpConfig of
    Nothing -> do
      Log.logInfo "Host assignment emails skipped (SMTP not configured)" (display $ Shows.id showModel)
      -- Still log what would have been sent for development
      forM_ hostIds $ \userId ->
        Log.logInfo "Would send host assignment email" (display userId, Shows.title showModel)
    Just smtpConfig ->
      forM_ hostIds $ \userId -> do
        sendHostAssignmentEmail smtpConfig showModel mTimeslot userId

-- | Send a single host assignment email.
sendHostAssignmentEmail ::
  SmtpConfig ->
  Shows.Model ->
  Maybe Text ->
  User.Id ->
  AppM ()
sendHostAssignmentEmail smtpConfig showModel mTimeslot userId = do
  -- Fetch user info
  execQuerySpan (UserMetadata.getUserWithMetadataById userId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch user for host assignment email" (display userId, Text.pack $ show err)
    Right Nothing -> do
      Log.logInfo "User not found for host assignment email" (display userId)
    Right (Just userWithMeta) -> do
      -- Build URLs using type-safe links
      let baseUrl = smtpBaseUrl smtpConfig
          showUrl = baseUrl <> "/" <> Text.pack (show $ Links.linkURI $ showsLinks.detail (Shows.slug showModel) Nothing)
          dashboardUrl = baseUrl <> "/" <> Text.pack (show $ Links.linkURI dashboardLinks.home)
          hostInfo =
            MailSender.HostAssignmentInfo
              { MailSender.haiHostName = display (UserMetadata.uwmDisplayName userWithMeta),
                MailSender.haiShowTitle = Shows.title showModel,
                MailSender.haiShowUrl = showUrl,
                MailSender.haiDashboardUrl = dashboardUrl,
                MailSender.haiTimeslot = mTimeslot
              }
          toEmail = display (UserMetadata.uwmEmail userWithMeta)
      MailSender.sendHostAssignmentEmailAsync smtpConfig toEmail hostInfo
      Log.logInfo "Queued host assignment email" (toEmail, Shows.title showModel)

--------------------------------------------------------------------------------

-- | Format a human-readable timeslot description from schedule data.
--
-- Example output: "Fridays 8:00 PM - 10:00 PM PT"
formatTimeslotDescription ::
  -- | Day of week
  DayOfWeek ->
  -- | Start time
  TimeOfDay ->
  -- | End time
  TimeOfDay ->
  Text
formatTimeslotDescription dow startTime endTime =
  let dayName = dayOfWeekPlural dow
      startStr = formatTimeOfDay startTime
      endStr = formatTimeOfDay endTime
   in [i|#{dayName} #{startStr} - #{endStr} PT|]

-- | Get the plural form of a day of week (e.g., "Fridays")
dayOfWeekPlural :: DayOfWeek -> Text
dayOfWeekPlural Monday = "Mondays"
dayOfWeekPlural Tuesday = "Tuesdays"
dayOfWeekPlural Wednesday = "Wednesdays"
dayOfWeekPlural Thursday = "Thursdays"
dayOfWeekPlural Friday = "Fridays"
dayOfWeekPlural Saturday = "Saturdays"
dayOfWeekPlural Sunday = "Sundays"

-- | Format a TimeOfDay as "8:00 PM"
formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay = Text.pack . formatTime defaultTimeLocale "%l:%M %p"
