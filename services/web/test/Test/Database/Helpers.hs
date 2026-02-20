{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Helpers
  ( insertTestUser,
    insertTestShowWithSchedule,
    insertTestBlogPost,
    insertTestShowBlogPost,
    insertTestEvent,
    insertTestEpisode,
    insertTestEphemeralUpload,
    insertTestStationId,
    addTestShowHost,
    verifyTestUserEmail,
    unwrapInsert,
  )
where

--------------------------------------------------------------------------------

import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Stack (HasCallStack)
import Hasql.Interpolate (OneRow (OneRow), interp, sql)
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

-- | Insert a blog post and return the blog post ID.
insertTestBlogPost :: BlogPosts.Insert -> TRX.Transaction BlogPosts.Id
insertTestBlogPost = unwrapInsert . BlogPosts.insertBlogPost

-- | Insert a show blog post and return the show blog post ID.
insertTestShowBlogPost :: ShowBlogPosts.Insert -> TRX.Transaction ShowBlogPosts.Id
insertTestShowBlogPost = unwrapInsert . ShowBlogPosts.insertShowBlogPost

-- | Insert an event and return the event ID.
insertTestEvent :: Events.Insert -> TRX.Transaction Events.Id
insertTestEvent = unwrapInsert . Events.insertEvent

-- | Insert an episode and return the episode ID.
insertTestEpisode :: Episodes.Insert -> TRX.Transaction Episodes.Id
insertTestEpisode = unwrapInsert . Episodes.insertEpisode

-- | Insert an ephemeral upload and return the upload ID.
insertTestEphemeralUpload :: EphemeralUploads.Insert -> TRX.Transaction EphemeralUploads.Id
insertTestEphemeralUpload = unwrapInsert . EphemeralUploads.insertEphemeralUpload

-- | Insert a station ID and return the station ID.
insertTestStationId :: StationIds.Insert -> TRX.Transaction StationIds.Id
insertTestStationId = unwrapInsert . StationIds.insertStationId

-- | Make a user a host of a show.
addTestShowHost :: Shows.Id -> User.Id -> TRX.Transaction ()
addTestShowHost showId userId =
  TRX.statement () (ShowHost.addHostToShow showId userId)

-- | Mark a user's email as verified (bypasses the token verification flow).
verifyTestUserEmail :: User.Id -> TRX.Transaction ()
verifyTestUserEmail userId =
  TRX.statement () $
    interp
      False
      [sql|
        UPDATE users
        SET email_verified = TRUE, email_verified_at = NOW()
        WHERE id = #{userId}
      |]
