{-# LANGUAGE RankNTypes #-}

module Test.Database.Property where

--------------------------------------------------------------------------------

import Control.Monad (join)
import Hedgehog
import Test.Hspec
import Test.Hspec.Hedgehog

--------------------------------------------------------------------------------

-- functions inspired by matt parsons blog:
-- parsonsmatt.org/2020/03/11/effectful_property_testing.html
arrange ::
  (forall x. m x -> IO x) ->
  PropertyT IO (m (PropertyT IO a)) ->
  PropertyT IO a
arrange unlift mkAction = do
  action <- mkAction
  join (evalIO (unlift action))

act :: m (PropertyT IO a) -> PropertyT IO (m (PropertyT IO a))
act = pure

assert :: (Monad m) => PropertyT IO a -> m (PropertyT IO a)
assert = pure

runs :: Int -> SpecWith a -> SpecWith a
runs i = modifyMaxSuccess (const i)
