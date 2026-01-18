{-# LANGUAGE PackageImports #-}

-- | Application monad specialized to KPBJ's custom context.
--
-- This module provides a concrete type alias for handlers, eliminating
-- the need for MTL-style constraint boilerplate across handler files.
module App.Monad (AppM) where

--------------------------------------------------------------------------------

import "web-server-core" App.Monad qualified as Core
import App.CustomContext (CustomContext)

--------------------------------------------------------------------------------

-- | Application monad specialized to 'CustomContext'.
--
-- Handlers use this instead of polymorphic @m@ with constraint lists.
-- All standard effects (MonadIO, MonadReader, MonadDB, etc.) are available
-- through the underlying 'Core.AppM' instances.
type AppM = Core.AppM CustomContext
