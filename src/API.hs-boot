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
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows (Status)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetLink :: Links.Link
staticGetLink :: Links.Link
mediaGetLink :: Links.Link
aboutGetLink :: Links.Link
archiveGetLink :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int64 -> Links.Link
blogGetLink :: Maybe Int64 -> Maybe Text -> Links.Link
blogNewGetLink :: Links.Link
blogNewPostLink :: Links.Link
blogPostGetLink :: Slug -> Links.Link
blogEditGetLink :: Slug -> Links.Link
blogEditPostLink :: Slug -> Links.Link
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
eventGetLink :: Slug -> Links.Link
eventEditGetLink :: Slug -> Links.Link
eventEditPostLink :: Slug -> Links.Link
showsGetLink :: Maybe PageNumber -> Maybe Genre -> Maybe Status -> Maybe Search -> Links.Link
showGetLink :: Slug -> Links.Link
showBlogGetLink :: Slug -> Maybe Int64 -> Maybe Text -> Links.Link
showBlogPostGetLink :: Slug -> Slug -> Links.Link
showBlogNewGetLink :: Slug -> Links.Link
showBlogNewPostLink :: Slug -> Links.Link
showBlogEditGetLink :: Slug -> Slug -> Links.Link
showBlogEditPostLink :: Slug -> Slug -> Links.Link
showBlogDeleteLink :: Slug -> Slug -> Links.Link
showEditGetLink :: Slug -> Links.Link
showEditPostLink :: Slug -> Links.Link
episodesGetLink :: Slug -> Slug -> Links.Link
episodesEditPostLink :: Slug -> Slug -> Links.Link
episodesNewGetLink :: Slug -> Links.Link
episodesNewPostLink :: Slug -> Links.Link
episodesEditGetLink :: Slug -> Slug -> Links.Link
episodesDeleteLink :: Slug -> Slug -> Links.Link
hostDashboardGetLink :: Maybe Slug -> Links.Link
