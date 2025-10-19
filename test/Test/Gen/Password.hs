module Test.Gen.Password where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.Password.Argon2
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genPassword :: (MonadIO m, MonadGen m) => m (PasswordHash Argon2)
genPassword = Gen.text (Range.linear 1 50) Gen.ascii >>= hashPassword . mkPassword
