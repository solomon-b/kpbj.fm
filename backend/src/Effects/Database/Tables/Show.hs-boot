module Effects.Database.Tables.Show where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text.Display (Display)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

newtype ShowId = ShowId Int64

instance Show ShowId

instance Eq ShowId

instance Ord ShowId

instance Num ShowId

instance Servant.FromHttpApiData ShowId

instance Servant.ToHttpApiData ShowId

instance ToJSON ShowId

instance FromJSON ShowId

instance Display ShowId

instance DecodeValue ShowId

instance EncodeValue ShowId
