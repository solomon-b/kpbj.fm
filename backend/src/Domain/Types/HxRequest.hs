module Domain.Types.HxRequest where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Servant qualified

--------------------------------------------------------------------------------

data HxRequest = IsHxRequest | IsNotHxRequest
  deriving (Show)

instance Servant.FromHttpApiData HxRequest where
  parseQueryParam = \case
    "true" -> Right IsHxRequest
    _ -> Right IsNotHxRequest

checkHxRequest :: HxRequest -> Bool
checkHxRequest = \case
  IsHxRequest -> True
  IsNotHxRequest -> False

foldHxReq :: Maybe HxRequest -> HxRequest
foldHxReq = fromMaybe IsNotHxRequest
