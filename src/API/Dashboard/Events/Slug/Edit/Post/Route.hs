module API.Dashboard.Events.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/events/:id/:slug/edit"
type Route =
  "dashboard"
    :> "events"
    :> Servant.Capture "id" Events.Id
    :> Servant.Capture "slug" Slug
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem EventEditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

-- | Form data for event editing
data EventEditForm = EventEditForm
  { eefTitle :: Text,
    eefDescription :: Text,
    eefStartsAt :: Text,
    eefEndsAt :: Text,
    eefLocationName :: Text,
    eefLocationAddress :: Text,
    eefStatus :: Text,
    eefPosterImage :: Maybe (FileData Mem),
    eefPosterImageClear :: Bool -- True if user explicitly removed the poster image
  }
  deriving (Show)

instance FromMultipart Mem EventEditForm where
  fromMultipart multipartData =
    EventEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> lookupInput "starts_at" multipartData
      <*> lookupInput "ends_at" multipartData
      <*> lookupInput "location_name" multipartData
      <*> lookupInput "location_address" multipartData
      <*> lookupInput "status" multipartData
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "poster_image" multipartData))
      <*> pure (parseClearFlag "poster_image_clear")
    where
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

      -- \| Parse a clear flag (true if the hidden input has value "true")
      parseClearFlag :: Text -> Bool
      parseClearFlag name = case lookupInput name multipartData of
        Right "true" -> True
        _ -> False

-- | Parse event status from text
parseStatus :: Text -> Maybe Events.Status
parseStatus "published" = Just Events.Published
parseStatus "draft" = Just Events.Draft
parseStatus _ = Nothing

-- | Parse datetime from HTML5 datetime-local format
parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" . Text.unpack
