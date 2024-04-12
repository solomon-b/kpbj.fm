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
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Exception (Exc (..))
import Control.Monad.Freer.Exception qualified as Exc
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.Reader qualified as Reader
import Control.Monad.Trans.Class (lift)
import Data.Has (Has)
import Data.Has qualified as Has
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as Auth.Server

--------------------------------------------------------------------------------
-- Helpers

simpleRelay :: (forall a. t a -> Freer.Eff r a) -> Freer.Eff (t ': r) v -> Freer.Eff r v
simpleRelay f = Freer.handleRelay pure (\e arr -> arr =<< f e)

mapError :: forall e1 e2 r a. (Freer.Member (Exc e2) r) => (e1 -> e2) -> Freer.Eff (Exc e1 ': r) a -> Freer.Eff r a
mapError f = simpleRelay $ \(Exc err) -> Exc.throwError (f err)

class ToServerError e where
  convertToServerError :: e -> Servant.ServerError

toServerError :: (ToServerError e, Freer.Member (Exc Servant.ServerError) r) => Freer.Eff (Exc e : r) a -> Freer.Eff r a
toServerError = mapError convertToServerError

instance (Freer.Member (Exc Servant.ServerError) r) => Auth.Server.ThrowAll (Freer.Eff r a) where
  throwAll = Exc.throwError

--------------------------------------------------------------------------------
-- Has Pattern

-- | View the contents of a @table@ in an 'MVar'. This is temporary until I
-- introduce an actual database.
viewTable :: forall env table r a. (Has (MVar table) env, Freer.Member (Log.LogT IO) r, Freer.Member (Reader env) r) => (table -> a) -> Freer.Eff r a
viewTable k = do
  mvar <- Reader.asks @env Has.getter
  Freer.send $ lift @Log.LogT $ MVar.withMVar mvar (pure . k)

-- | Update the contents of a @table@ in an 'MVar'. This is temporary until I
-- introduce an actual database.
overTable ::
  forall env table r.
  (Has (MVar table) env, Freer.Member (Log.LogT IO) r, Freer.Member (Reader env) r) =>
  (table -> table) ->
  Freer.Eff r ()
overTable k = do
  mvar <- Reader.asks @env Has.getter
  Freer.send $ lift @Log.LogT $ MVar.modifyMVar_ mvar (pure . k)

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
