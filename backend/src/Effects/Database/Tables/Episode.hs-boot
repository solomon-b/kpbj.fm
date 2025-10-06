module Effects.Database.Tables.Episode where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text.Display (Display)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

newtype EpisodeId = EpisodeId Int64

instance Show EpisodeId

instance Eq EpisodeId

instance Ord EpisodeId

instance Num EpisodeId

instance Servant.FromHttpApiData EpisodeId

instance Servant.ToHttpApiData EpisodeId

instance ToJSON EpisodeId

instance FromJSON EpisodeId

instance Display EpisodeId

instance DecodeValue EpisodeId

instance EncodeValue EpisodeId
