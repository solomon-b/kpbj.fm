module Domain.Types.HxRequest where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Servant qualified

--------------------------------------------------------------------------------

data HxRequest = IsHxRequest | IsNotHxRequest
  deriving (Show, Eq)

instance Servant.FromHttpApiData HxRequest where
  parseQueryParam = \case
    "true" -> Right IsHxRequest
    _ -> Right IsNotHxRequest

foldHxReq :: Maybe HxRequest -> HxRequest
foldHxReq = fromMaybe IsNotHxRequest
