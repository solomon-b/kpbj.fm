module API.Dashboard.Events.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/events/new"
    ( "dashboard"
        :> "events"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem NewEventForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Parse and sanitize comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText

-- Form data structure
data NewEventForm = NewEventForm
  { nefTitle :: Text,
    nefTags :: Text,
    nefDescription :: Text,
    nefStartsAt :: Text,
    nefEndsAt :: Text,
    nefLocationName :: Text,
    nefLocationAddress :: Text,
    nefStatus :: Text,
    nefPosterImage :: Maybe (FileData Mem)
  }
  deriving (Show)

instance FromMultipart Mem NewEventForm where
  fromMultipart multipartData =
    NewEventForm
      <$> lookupInput "title" multipartData
      <*> pure (fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> lookupInput "description" multipartData
      <*> lookupInput "starts_at" multipartData
      <*> lookupInput "ends_at" multipartData
      <*> lookupInput "location_name" multipartData
      <*> lookupInput "location_address" multipartData
      <*> lookupInput "status" multipartData
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "poster_image" multipartData))
    where
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing
