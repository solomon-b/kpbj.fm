module Test.Gen.Tables.User where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Test.Gen.EmailAddress (genEmail)
import Test.Gen.Password (genPassword)

--------------------------------------------------------------------------------

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miPassword <- genPassword
  pure User.ModelInsert {..}
