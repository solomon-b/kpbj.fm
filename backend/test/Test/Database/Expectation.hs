{-# LANGUAGE ViewPatterns #-}

module Test.Database.Expectation where

--------------------------------------------------------------------------------

import Control.Monad
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.Time
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack)
import Test.Hspec.Expectations (Expectation, expectationFailure)

--------------------------------------------------------------------------------

expectTrue :: (HasCallStack) => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

assertRight :: (Show e) => Either e v -> IO v
assertRight = \case
  Left e -> assertFailure $ "Expected Right but got Left " <> show e
  Right v -> pure v

assertJust :: Maybe a -> IO a
assertJust = \case
  Nothing -> assertFailure "Expected Just but got Nothing"
  Just v -> pure v

assertCloseTo :: UTCTime -> UTCTime -> IO ()
assertCloseTo t1 t2 =
  let msg = "UTCTimes were not close enough: " <> show t1 <> " " <> show t2
   in expectTrue msg $ isCloseTo t1 t2

isCloseTo :: UTCTime -> UTCTime -> Bool
isCloseTo t1 t2 =
  let eps = 1e-5
   in abs (diffUTCTime t1 t2) < eps

hasSameElements :: (Foldable f1, Foldable f2, Eq a) => f1 a -> f2 a -> Bool
hasSameElements (F.toList -> xs) (F.toList -> ys) = null (xs List.\\ ys) && null (ys List.\\ xs)
