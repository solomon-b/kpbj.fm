module API where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.Genre (Genre)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.PageView (PageView)
import Domain.Types.Search (Search)
import Effects.Database.Tables.Shows (Status)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetLink :: Links.Link
staticGetLink :: Links.Link
mediaGetLink :: Links.Link
aboutGetLink :: Links.Link
blogGetLink :: Maybe Int64 -> Maybe Text -> Links.Link
blogNewGetLink :: Links.Link
blogNewPostLink :: Links.Link
blogPostGetLink :: Text -> Links.Link
blogEditGetLink :: Text -> Links.Link
blogEditPostLink :: Text -> Links.Link
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
showsGetLink :: Maybe PageNumber -> Maybe Genre -> Maybe Status -> Maybe Search -> Links.Link
showGetLink :: Text -> Links.Link
showEditGetLink :: Text -> Links.Link
showEditPostLink :: Text -> Links.Link
episodesGetLink :: Text -> Text -> Links.Link
episodesEditPostLink :: Text -> Text -> Links.Link
episodeUploadGetLink :: Text -> Links.Link
episodeUploadPostLink :: Text -> Links.Link
episodesEditGetLink :: Text -> Text -> Links.Link
hostDashboardGetLink :: Links.Link
