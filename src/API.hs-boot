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
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- Admin routes
adminUsersGetLink :: Maybe Int64 -> Maybe Text -> Maybe UserMetadata.UserRole -> Links.Link
adminUserDetailGetLink :: User.Id -> Links.Link
adminUserEditGetLink :: User.Id -> Links.Link
adminUserEditPostLink :: User.Id -> Links.Link
adminUserDeleteLink :: User.Id -> Links.Link

rootGetLink :: Links.Link
staticGetLink :: Links.Link
mediaGetLink :: Links.Link
aboutGetLink :: Links.Link
archiveGetLink :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int64 -> Links.Link
blogGetLink :: Maybe Int64 -> Maybe Text -> Links.Link
blogNewGetLink :: Links.Link
blogNewPostLink :: Links.Link
blogPostGetLink :: BlogPosts.Id -> Slug -> Links.Link
blogPostGetLinkById :: BlogPosts.Id -> Links.Link
blogEditGetLink :: BlogPosts.Id -> Slug -> Links.Link
blogEditPostLink :: BlogPosts.Id -> Slug -> Links.Link
blogDeleteLink :: BlogPosts.Id -> Slug -> Links.Link
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
eventGetLink :: Events.Id -> Slug -> Links.Link
eventGetLinkById :: Events.Id -> Links.Link
eventEditGetLink :: Events.Id -> Slug -> Links.Link
eventEditPostLink :: Events.Id -> Slug -> Links.Link
eventDeleteLink :: Events.Id -> Slug -> Links.Link
showsGetLink :: Maybe PageNumber -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Links.Link
showsScheduleGetLink :: Maybe WeekOffset -> Links.Link
showGetLink :: Slug -> Links.Link
showBlogGetLink :: Slug -> Maybe Int64 -> Maybe Text -> Links.Link
showBlogPostGetLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogPostGetLinkById :: Shows.Id -> ShowBlogPosts.Id -> Links.Link
showBlogNewGetLink :: Slug -> Links.Link
showBlogNewPostLink :: Slug -> Links.Link
showBlogEditGetLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogEditPostLink :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.Link
showBlogDeleteLink :: Shows.Id -> Slug -> ShowBlogPosts.Id -> Slug -> Links.Link
showEditGetLink :: Slug -> Links.Link
showEditPostLink :: Slug -> Links.Link
episodesGetLink :: Shows.Id -> Episodes.Id -> Slug -> Links.Link
episodesGetLinkById :: Shows.Id -> Episodes.Id -> Links.Link
episodesEditPostLink :: Shows.Id -> Episodes.Id -> Slug -> Links.Link
episodesNewGetLink :: Slug -> Links.Link
episodesNewPostLink :: Slug -> Links.Link
episodesEditGetLink :: Shows.Id -> Episodes.Id -> Slug -> Links.Link
episodesDeleteLink :: Shows.Id -> Slug -> Episodes.Id -> Slug -> Links.Link
hostDashboardGetLink :: Maybe Slug -> Links.Link
