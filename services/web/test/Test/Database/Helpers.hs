module Test.Database.Helpers (insertTestUser, insertTestShowWithSchedule) where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX

--------------------------------------------------------------------------------

-- | Insert a user (with metadata) and return the user ID.
insertTestUser :: UserMetadata.UserWithMetadataInsert -> TRX.Transaction User.Id
insertTestUser um = do
  let userInsert = User.ModelInsert (UserMetadata.uwmiEmail um) (UserMetadata.uwmiPassword um)
  (OneRow userId) <- TRX.statement () $ User.insertUser userInsert
  let metaInsert =
        UserMetadata.Insert
          userId
          (UserMetadata.uwmiDisplayName um)
          (UserMetadata.uwmiFullName um)
          (UserMetadata.uwmiAvatarUrl um)
          (UserMetadata.uwmiUserRole um)
          (UserMetadata.uwmiColorScheme um)
          (UserMetadata.uwmiTheme um)
  _ <- TRX.statement () $ UserMetadata.insertUserMetadata metaInsert
  pure userId

-- | Insert a show with a schedule template and return (showId, templateId).
insertTestShowWithSchedule ::
  Shows.Insert ->
  ShowSchedule.ScheduleTemplateInsert ->
  TRX.Transaction (Shows.Id, ShowSchedule.TemplateId)
insertTestShowWithSchedule showInsert scheduleTemplate = do
  showId <- TRX.statement () (Shows.insertShow showInsert)
  let scheduleWithShowId = scheduleTemplate {ShowSchedule.stiShowId = showId}
  templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleWithShowId)
  pure (showId, templateId)
