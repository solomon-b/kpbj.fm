module Utils where

--------------------------------------------------------------------------------

import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------

fromRightM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
fromRightM f m = either f pure =<< m

catEithers :: [Either e a] -> [a]
catEithers = mapMaybe (either (const Nothing) Just)
