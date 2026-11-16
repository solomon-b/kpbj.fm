{-# LANGUAGE OverloadedRecordDot #-}

-- | Schedule conflict check for host invitations.
--
-- New, regenerated, and edited invitations use this check. It runs before the
-- invitation is stored, so staff see a conflict before they send the link.
module API.Dashboard.Invitations.Conflict
  ( checkInvitationConflicts,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), checkScheduleConflicts, formatTimeHHMM)
import API.Invite.Token.Post.Handler (parseInvitationSchedules)
import App.Handler.Error (HandlerError, throwDatabaseError, throwValidationError)
import App.Monad (AppM)
import Control.Monad (forM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, TimeOfDay)
import Domain.Types.Recurrence (recurrenceDay, weekNumbers)
import Domain.Types.Timezone (addMinutesToTimeOfDay, slotDurationMins)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Fail when the slot overlaps a scheduled show or a live invitation.
--
-- Scheduled shows are checked from @startsOn@, so a show that leaves the slot
-- before that date is not a conflict. A live invitation has no end date, so any
-- live invitation that holds an overlapping slot is a conflict, whatever its
-- start date.
--
-- An edit passes the invitation that it changes. That invitation holds its old
-- slot until the save, so the check ignores it.
checkInvitationConflicts ::
  -- | The invitation that an edit changes, which the check ignores
  Maybe HostInvitation.Id ->
  -- | The slot the invitation holds after the save
  Maybe ParsedScheduleSlot ->
  -- | The date the slot takes effect
  Day ->
  ExceptT HandlerError AppM ()
checkInvitationConflicts _ Nothing _ = pure ()
checkInvitationConflicts edited (Just slot) startsOn = do
  showConflict <- lift $ checkScheduleConflicts (Shows.Id 0) (Just slot) startsOn
  either throwValidationError pure showConflict

  live <- fromRightM throwDatabaseError $ execQuery HostInvitation.getLive
  let candidates = mapMaybe invitationCandidate (filter (\inv -> Just inv.hiId /= edited) live)
      checkRange prefix start end = do
        mLabel <-
          fromRightM throwDatabaseError $
            execQuery (ShowSchedule.checkSlotConflictAgainst candidates day weeks start end)
        forM_ mLabel $ \label ->
          throwValidationError (conflictMessage prefix start end label)

  checkRange "Schedule conflict" (pssStart slot) (pssEnd slot)
  forM_ (pssReplayStartTime slot) $ \replayStart ->
    checkRange "Replay conflict" replayStart (addMinutesToTimeOfDay replayStart (slotDurationMins (pssStart slot) (pssEnd slot)))
  where
    day = recurrenceDay (pssRecurrence slot)
    weeks = weekNumbers (pssRecurrence slot)

    conflictMessage :: Text -> TimeOfDay -> TimeOfDay -> Text -> Text
    conflictMessage prefix start end label =
      prefix <> ": " <> Text.pack (show day) <> " " <> formatTimeHHMM start <> "-" <> formatTimeHHMM end <> " overlaps with " <> label

-- | The slot a live invitation holds, labelled by its recipient.
--
-- An invitation whose schedule does not parse, or that holds no slot, gives
-- nothing. The new invitation form rejects both, so neither is stored today.
invitationCandidate :: HostInvitation.Model -> Maybe ShowSchedule.CandidateSlot
invitationCandidate inv = case parseInvitationSchedules inv.hiScheduleData of
  Right (Just slot) ->
    Just
      ShowSchedule.CandidateSlot
        { ShowSchedule.csLabel = "a pending invitation for " <> display inv.hiRecipientEmail,
          ShowSchedule.csDayOfWeek = recurrenceDay (pssRecurrence slot),
          ShowSchedule.csWeeksOfMonth = weekNumbers (pssRecurrence slot),
          ShowSchedule.csStartTime = pssStart slot,
          ShowSchedule.csEndTime = pssEnd slot,
          ShowSchedule.csReplayStartTime = pssReplayStartTime slot
        }
  _ -> Nothing
