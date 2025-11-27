{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Filter
  ( Filter (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Servant qualified

--------------------------------------------------------------------------------

-- | Newtype wrapper for optional query parameter filtering.
--
-- Treats empty string as 'Nothing' instead of a parse error.
-- This is needed because HTML forms send empty string for unselected options,
-- but Servant's QueryParam treats empty string as a parse error (400).
--
-- Usage: Change @QueryParam "role" UserRole@ to @QueryParam "role" (Filter UserRole)@
-- Handler receives @Maybe (Filter UserRole)@ which can be unwrapped with 'getFilter'.
newtype Filter a = Filter {getFilter :: Maybe a}
  deriving stock (Show, Eq)

instance (Servant.FromHttpApiData a) => Servant.FromHttpApiData (Filter a) where
  parseUrlPiece t
    | Text.null (Text.strip t) = Right (Filter Nothing)
    | otherwise = Filter . Just <$> Servant.parseUrlPiece t

instance (Servant.ToHttpApiData a) => Servant.ToHttpApiData (Filter a) where
  toUrlPiece (Filter Nothing) = ""
  toUrlPiece (Filter (Just a)) = Servant.toUrlPiece a
