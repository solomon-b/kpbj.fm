module Test.Database.Property.Assert where

--------------------------------------------------------------------------------

import Hedgehog
import Hedgehog.Internal.Property (Diff (..), failWith)
import Hedgehog.Internal.Show (Value (..), ValueDiff (..), showPretty)
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import Test.Database.Expectation (hasSameElements)

--------------------------------------------------------------------------------

assertRight :: (Show e) => Either e v -> PropertyT IO v
assertRight = \case
  Left e -> do
    annotate "unexpected Left:"
    annotateShow e
    failure
  Right v -> pure v

assertJust :: Maybe v -> PropertyT IO v
assertJust = \case
  Nothing -> annotate "Expected a Just but found Nothing" >> failure
  Just v -> pure v

assertNothing :: (Show v) => Maybe v -> PropertyT IO ()
assertNothing = \case
  Nothing -> pure ()
  Just v -> do
    annotate "Expected Nothing but found Just:"
    annotateShow v
    failure

(<==) :: (MonadTest m, Show a, HasCallStack) => a -> (a -> Bool) -> m ()
(<==) val predicate = withFrozenCallStack $ do
  ok <- withFrozenCallStack $ eval $ predicate val
  if ok
    then success
    else
      withFrozenCallStack $
        failWith
          ( Just $
              Diff
                { diffPrefix = "━━━ ",
                  diffRemoved = mempty,
                  diffInfix = "Does not pass predicate",
                  diffAdded = mempty,
                  diffSuffix = " ━━━",
                  diffValue = ValueSame $ String $ showPretty val
                }
          )
          ""

(==>) :: (MonadTest m, Show a, HasCallStack) => (a -> Bool) -> a -> m ()
(==>) predicate val = withFrozenCallStack $ val <== predicate

(-<-) :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
(-<-) x y =
  withFrozenCallStack $
    diff x (<) y

(->-) :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
(->-) x y =
  withFrozenCallStack $
    diff x (>) y

(=>=) :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
(=>=) x y =
  withFrozenCallStack $
    diff x (>=) y

(=<=) :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
(=<=) x y =
  withFrozenCallStack $
    diff x (<=) y

(=\\=) :: (MonadTest m, Eq a, Show a, HasCallStack) => [a] -> [a] -> m ()
(=\\=) xs ys =
  withFrozenCallStack $
    diff xs hasSameElements ys
