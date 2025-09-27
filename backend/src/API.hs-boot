module API where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.PageView
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetLink :: Links.Link
staticGetLink :: Links.Link
aboutGetLink :: Links.Link
blogGetLink :: Maybe Int64 -> Maybe Text -> Maybe Text -> Links.Link
blogNewGetLink :: Links.Link
blogNewPostLink :: Links.Link
blogPostGetLink :: Text -> Links.Link
donateGetLink :: Links.Link
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userLoginPostLink :: Maybe Text -> Links.Link
userLogoutGetLink :: Links.Link
userLogoutPostLink :: Links.Link
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> Links.Link
userRegisterPostLink :: Links.Link
privacyPolicyGetLink :: Links.Link
termsOfServiceGetLink :: Links.Link
eventsGetLink :: Maybe Text -> Maybe PageView -> Links.Link
eventsNewGetLink :: Links.Link
eventsNewPostLink :: Links.Link
eventGetLink :: Text -> Links.Link
showsGetLink :: Maybe Int64 -> Maybe Text -> Maybe Text -> Links.Link
showGetLink :: Text -> Links.Link
episodeUploadGetLink :: Links.Link
episodeUploadPostLink :: Links.Link
