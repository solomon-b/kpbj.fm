module API.Dashboard.Events.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/events/new"
type Route =
  "dashboard"
    :> "events"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem NewEventForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

-- Form data structure
data NewEventForm = NewEventForm
  { nefTitle :: Text,
    nefDescription :: Text,
    nefStartsAt :: Text,
    nefEndsAt :: Text,
    nefLocationName :: Text,
    nefLocationAddress :: Text,
    nefStatus :: Text,
    nefPosterImage :: Maybe (FileData Mem),
    nefFeaturedOnHomepage :: Text
  }
  deriving (Show)

instance FromMultipart Mem NewEventForm where
  fromMultipart multipartData =
    NewEventForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> lookupInput "starts_at" multipartData
      <*> lookupInput "ends_at" multipartData
      <*> lookupInput "location_name" multipartData
      <*> lookupInput "location_address" multipartData
      <*> lookupInput "status" multipartData
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "poster_image" multipartData))
      <*> lookupInput "featured_on_homepage" multipartData
    where
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing
