{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Effects.Logger where

--------------------------------------------------------------------------------

import Control.Monad.Freer qualified as Freer
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Log qualified

--------------------------------------------------------------------------------

logInfo :: (Freer.Member Logger r, Aeson.ToJSON a) => Text -> a -> Freer.Eff r ()
logInfo msg = Freer.send . LogInfo msg

logTrace :: (Freer.Member Logger r, Aeson.ToJSON a) => Text -> a -> Freer.Eff r ()
logTrace msg = Freer.send . LogTrace msg

logAttention :: (Freer.Member Logger r, Aeson.ToJSON a) => Text -> a -> Freer.Eff r ()
logAttention msg = Freer.send . LogAttention msg

data Logger a where
  LogInfo :: (Aeson.ToJSON x) => Text -> x -> Logger ()
  LogTrace :: (Aeson.ToJSON x) => Text -> x -> Logger ()
  LogAttention :: (Aeson.ToJSON x) => Text -> x -> Logger ()

runLogger :: (Freer.Member (Log.LogT IO) r) => Freer.Eff (Logger ': r) v -> Freer.Eff r v
runLogger =
  Freer.runNat @(Log.LogT IO) $
    \case
      LogInfo msg x ->
        Log.logInfo msg x
      LogTrace msg x ->
        Log.logTrace msg x
      LogAttention msg x ->
        Log.logAttention msg x
