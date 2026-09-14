{-# LANGUAGE ViewPatterns #-}

module Test.Database.Expectation where

--------------------------------------------------------------------------------

import Data.Foldable qualified as F
import Data.List qualified as List

--------------------------------------------------------------------------------

hasSameElements :: (Foldable f1, Foldable f2, Eq a) => f1 a -> f2 a -> Bool
hasSameElements (F.toList -> xs) (F.toList -> ys) = null (xs List.\\ ys) && null (ys List.\\ xs)
