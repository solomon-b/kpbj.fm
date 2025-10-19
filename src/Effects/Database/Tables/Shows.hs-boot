module Effects.Database.Tables.Shows where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text.Display (Display)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

newtype Id = Id Int64

instance Show Id

instance Eq Id

instance Ord Id

instance Num Id

instance Servant.FromHttpApiData Id

instance Servant.ToHttpApiData Id

instance ToJSON Id

instance FromJSON Id

instance Display Id

instance DecodeValue Id

instance EncodeValue Id
