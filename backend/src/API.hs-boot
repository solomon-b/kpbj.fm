module API where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
