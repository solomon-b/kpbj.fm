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
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LT
import Data.Time (DayOfWeek (..), TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Email.Send qualified as Email
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
sendHostAssignmentNotifications showModel mTimeslot hostIds =
  forM_ hostIds $ \userId ->
    sendHostAssignmentEmail showModel mTimeslot userId

-- | Send a single host assignment email.
sendHostAssignmentEmail ::
  Shows.Model ->
  Maybe Text ->
  User.Id ->
  AppM ()
sendHostAssignmentEmail showModel mTimeslot userId = do
  execQuery (UserMetadata.getUserWithMetadataById userId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch user for host assignment email" (display userId, Text.pack $ show err)
    Right Nothing -> do
      Log.logInfo "User not found for host assignment email" (display userId)
    Right (Just userWithMeta) -> do
      url <- Email.baseUrl
      let showUrl = url <> "/" <> Text.pack (show $ Links.linkURI $ showsLinks.detail (Shows.slug showModel) Nothing)
          dashboardUrl = url <> "/" <> Text.pack (show $ Links.linkURI dashboardLinks.home)
          hostInfo =
            HostAssignmentInfo
              { haiHostName = display (UserMetadata.uwmDisplayName userWithMeta),
                haiShowTitle = Shows.title showModel,
                haiShowUrl = showUrl,
                haiDashboardUrl = dashboardUrl,
                haiTimeslot = mTimeslot
              }
          toEmail = display (UserMetadata.uwmEmail userWithMeta)
      Email.sendAsync (buildHostAssignmentEmail toEmail hostInfo)
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

--------------------------------------------------------------------------------

-- | Information needed to send a host assignment notification email.
data HostAssignmentInfo = HostAssignmentInfo
  { -- | The host's display name
    haiHostName :: Text,
    -- | The name of the show
    haiShowTitle :: Text,
    -- | Full URL to the show page (built with safe links)
    haiShowUrl :: Text,
    -- | Full URL to the host dashboard (built with safe links)
    haiDashboardUrl :: Text,
    -- | Human-readable timeslot description (e.g., "Fridays 8:00 PM - 10:00 PM PT")
    haiTimeslot :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | Build a host assignment notification email.
buildHostAssignmentEmail ::
  -- | Recipient email address
  Text ->
  -- | Host assignment information
  HostAssignmentInfo ->
  Email.Email
buildHostAssignmentEmail toEmail HostAssignmentInfo {..} =
  let timeslotText = fromMaybe "TBD" haiTimeslot
   in Email.Email
        { Email.emailTo = toEmail,
          Email.emailSubject = "Welcome to " <> haiShowTitle <> " - KPBJ 95.9FM",
          Email.emailBody =
            LT.fromStrict
              [i|
================================================================
                  KPBJ 95.9FM COMMUNITY RADIO
================================================================

Welcome to KPBJ Radio, #{haiHostName}!

You've been added as a host for "#{haiShowTitle}".

YOUR TIMESLOT
-------------
#{timeslotText}

WHAT'S NEXT
-----------
1. Visit your host dashboard:
   #{haiDashboardUrl}

2. Check out your show page:
   #{haiShowUrl}

3. Start preparing your episodes!
   Upload audio, add track listings, write blog posts.

NEED HELP?
----------
Questions about hosting? Reach out to station staff.

You can find us on Discord: https://discord.gg/Pt5pnK7X

Or email us at contact@kpbj.fm

We're excited to have you on the airwaves!

--
The KPBJ Team
https://www.kpbj.fm

================================================================
|],
          Email.emailLabel = "host-assignment"
        }
