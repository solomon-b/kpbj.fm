module Utils where

--------------------------------------------------------------------------------

import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m

catEithers :: [Either e a] -> [a]
catEithers = mapMaybe (either (const Nothing) Just)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (ls, rs) = (a : ls, rs)
    right b (ls, rs) = (ls, b : rs)
