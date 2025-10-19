module Test.Gen.Tables.UserMetadata where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Test.Gen.DomainTypes (genDisplayName, genFullName)
import Test.Gen.EmailAddress (genEmail)
import Test.Gen.Password (genPassword)
import Test.Gen.Text (genUrl)

--------------------------------------------------------------------------------

genUserRole :: (MonadGen m) => m UserMetadata.UserRole
genUserRole = Gen.enumBounded

userMetadataInsertGen :: (MonadIO m, MonadGen m) => User.Id -> m UserMetadata.ModelInsert
userMetadataInsertGen userId = do
  let miUserId = userId
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miAvatarUrl <- Gen.maybe genUrl
  miUserRole <- genUserRole
  pure UserMetadata.ModelInsert {..}

userWithMetadataInsertGen :: (MonadIO m, MonadGen m) => m UserMetadata.UserWithMetadataInsert
userWithMetadataInsertGen = do
  uwmiEmail <- genEmail
  uwmiPassword <- genPassword
  uwmiDisplayName <- genDisplayName
  uwmiFullName <- genFullName
  uwmiAvatarUrl <- Gen.maybe genUrl
  uwmiUserRole <- genUserRole
  pure UserMetadata.UserWithMetadataInsert {..}
