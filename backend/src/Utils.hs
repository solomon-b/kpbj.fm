{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

--------------------------------------------------------------------------------

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Except (MonadError, mapError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader'
import Control.Monad.Trans.Class (lift)
import Data.Has (Has)
import Data.Has qualified as Has
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as Auth.Server

--------------------------------------------------------------------------------
-- Error Handling

class ToServerError e where
  convertToServerError :: e -> Servant.ServerError

-- toServerError :: (ToServerError e, MonadError e n, MonadError Servant.ServerError m) => n a -> m a
-- toServerError = mapError _ --convertToServerError

--------------------------------------------------------------------------------
-- Has Pattern

-- | View the contents of a @table@ in an 'MVar'. This is temporary until I
-- introduce an actual database.
viewTable :: forall env table m a. (Has (MVar table) env, Log.MonadLog m, MonadReader env m, MonadIO m) => (table -> a) -> m a
viewTable k = do
  mvar <- Reader'.asks @env Has.getter
  liftIO $ MVar.withMVar mvar (pure . k)

-- | Update the contents of a @table@ in an 'MVar'. This is temporary until I
-- introduce an actual database.
overTable ::
  forall env table m.
  (Has (MVar table) env, Log.MonadLog m, MonadReader env m, MonadIO m) =>
  (table -> table) ->
  m ()
overTable k = do
  mvar <- Reader'.asks @env Has.getter
  liftIO $ MVar.modifyMVar_ mvar (pure . k)

--------------------------------------------------------------------------------
-- Variants

-- injectError :: forall e es r a. (Member (Exc (Variant es)) r, CouldBe es e) => Eff (Exc e : r) a -> Eff r a
-- injectError = mapError (throw :: e -> Variant es)

-- data Variant xs where
--   Here  :: x -> Variant (x : xs)
--   There :: Variant xs -> Variant (x : xs)

-- class CouldBe xs x  where
--   inject  :: x -> Variant xs
--   project :: Variant xs -> Maybe x

-- throw :: CouldBe xs x => x -> Variant xs
-- throw = inject

-- instance {-# OVERLAPS #-} CouldBe (x : xs) x where
--   inject :: x -> Variant (x : xs)
--   inject = Here

--   project :: Variant (x : xs) -> Maybe x
--   project = \case
--     Here x -> Just x
--     There _variant -> Nothing

-- instance {-# OVERLAPPABLE #-} CouldBe xs y => CouldBe (x : xs) y where
--   inject :: CouldBe xs y => y -> Variant (x : xs)
--   inject y = There (inject y)

--   project :: CouldBe xs y => Variant (x : xs) -> Maybe y
--   project = \case
--     Here _ -> Nothing
--     There variant -> project variant
