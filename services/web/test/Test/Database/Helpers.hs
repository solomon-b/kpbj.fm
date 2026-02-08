{-# LANGUAGE LambdaCase #-}

module Test.Database.Helpers (insertTestUser, insertTestShowWithSchedule, unwrapInsert) where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Stack (HasCallStack)
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX

--------------------------------------------------------------------------------

-- | Unwrap a @Maybe@ result from an insert statement, failing the test if @Nothing@.
--
-- Use this for test setup where an insert returning @Nothing@ means the test
-- infrastructure is broken, not a meaningful test failure.
unwrapInsert :: (HasCallStack) => Hasql.Statement () (Maybe a) -> TRX.Transaction a
unwrapInsert stmt =
  TRX.statement () stmt >>= \case
    Nothing -> error "unwrapInsert: insert returned Nothing (test setup failure)"
    Just x -> pure x

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
  showId <- unwrapInsert (Shows.insertShow showInsert)
  let scheduleWithShowId = scheduleTemplate {ShowSchedule.stiShowId = showId}
  templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleWithShowId)
  pure (showId, templateId)
