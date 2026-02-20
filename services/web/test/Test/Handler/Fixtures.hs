-- | Shared test fixtures for handler integration tests.
--
-- Provides common helper functions for creating test users, schedule templates,
-- and unwrapping test setup results. Import this module instead of defining
-- these helpers locally in each test file.
module Test.Handler.Fixtures
  ( mkUserInsert,
    expectSetupRight,
    defaultScheduleInsert,
    setupUserModels,
    nonExistentId,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Password.Argon2 (hashPassword, mkPassword)
import Data.Text (Text)
import Data.Time (TimeOfDay (..))
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.FullName (mkFullNameUnsafe)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Stack (HasCallStack)
import Hasql.Transaction qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.HUnit (assertFailure)

--------------------------------------------------------------------------------

-- | Create a user insert with a unique email prefix and the given role.
mkUserInsert :: Text -> UserMetadata.UserRole -> IO UserMetadata.UserWithMetadataInsert
mkUserInsert emailPrefix role = do
  pass <- hashPassword $ mkPassword "testpassword"
  pure
    UserMetadata.UserWithMetadataInsert
      { uwmiEmail = mkEmailAddress $ emailPrefix <> "@test.example.com",
        uwmiPassword = pass,
        uwmiDisplayName = mkDisplayNameUnsafe "Test User",
        uwmiFullName = mkFullNameUnsafe "Test User",
        uwmiAvatarUrl = Nothing,
        uwmiUserRole = role,
        uwmiColorScheme = UserMetadata.Automatic,
        uwmiTheme = UserMetadata.DefaultTheme
      }

-- | Unwrap a Right value or fail the test with a descriptive message.
expectSetupRight :: (Show e, MonadIO m) => Either e a -> m a
expectSetupRight = \case
  Left err -> liftIO $ assertFailure $ "Test setup failed: " <> show err
  Right a -> pure a

-- | Default schedule template insert (show ID is replaced by helper).
defaultScheduleInsert :: ShowSchedule.ScheduleTemplateInsert
defaultScheduleInsert =
  ShowSchedule.ScheduleTemplateInsert
    { stiShowId = Shows.Id 0,
      stiDayOfWeek = Nothing,
      stiWeeksOfMonth = Nothing,
      stiStartTime = TimeOfDay 10 0 0,
      stiEndTime = TimeOfDay 11 0 0,
      stiTimezone = "America/Los_Angeles",
      stiAirsTwiceDaily = False
    }

-- | Set up a user from a pre-built insert and return User.Model + UserMetadata.Model.
--
-- The 'UserWithMetadataInsert' must be prepared outside the transaction
-- because 'TRX.Transaction' does not have a 'MonadIO' instance.
setupUserModels ::
  (HasCallStack) =>
  UserMetadata.UserWithMetadataInsert ->
  TRX.Transaction (User.Model, UserMetadata.Model)
setupUserModels userInsert = do
  userId <- insertTestUser userInsert
  userModel <- TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
  userMetaModel <- TRX.statement () (UserMetadata.getUserMetadata userId) >>= maybe (error "metadata not found") pure
  pure (userModel, userMetaModel)

-- | A nonexistent ID value safe to use in "not found" tests.
--
-- PostgreSQL serial IDs start at 1 and will not reach this value in tests.
nonExistentId :: Int64
nonExistentId = 999999999
