module API.Dashboard.Events.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Tables.Events qualified as Events
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/events/:id/:slug/edit"
    ( "dashboard"
        :> "events"
        :> Servant.Capture "id" Events.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem EventEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

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
    eefTags :: [Text],
    eefPosterImage :: Maybe (FileData Mem)
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
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "poster_image" multipartData))
    where
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText

-- | Parse event status from text
parseStatus :: Text -> Maybe Events.Status
parseStatus "published" = Just Events.Published
parseStatus "draft" = Just Events.Draft
parseStatus _ = Nothing

-- | Parse datetime from HTML5 datetime-local format
parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" . Text.unpack
