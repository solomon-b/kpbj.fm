{-# LANGUAGE OverloadedRecordDot #-}

module Test.Gen.Tables.ShowHost where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------

hostRoleGen :: Hedgehog.Gen ShowHost.HostRole
hostRoleGen = Gen.enumBounded

showHostInsertGen :: Shows.Id -> User.Id -> Hedgehog.Gen ShowHost.Insert
showHostInsertGen showId userId = do
  role <- hostRoleGen
  isPrimary <- Gen.bool
  pure $ ShowHost.Insert showId userId role isPrimary
