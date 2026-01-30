module API.Dashboard.StationBlog.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | "POST /dashboard/station-blog/:id/:slug/edit"
type Route =
  "dashboard"
    :> "station-blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> Servant.Capture "slug" Slug
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem BlogEditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

-- | Form data for blog post editing
data BlogEditForm = BlogEditForm
  { befTitle :: Text,
    befContent :: Text,
    befExcerpt :: Maybe Text,
    befStatus :: Text,
    befTags :: [Text],
    befHeroImage :: Maybe (FileData Mem),
    befHeroImageClear :: Bool -- True if user explicitly removed the hero image
  }
  deriving (Show)

instance FromMultipart Mem BlogEditForm where
  fromMultipart multipartData =
    BlogEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "content" multipartData
      <*> pure (emptyToNothing $ either (const Nothing) Just (lookupInput "excerpt" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "hero_image" multipartData))
      <*> pure (parseClearFlag "hero_image_clear")
    where
      -- \| Convert empty text to Nothing
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
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

instance FromForm BlogEditForm where
  fromForm :: Form.Form -> Either Text BlogEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    content <- Form.parseUnique "content" form
    excerpt <- Form.parseMaybe "excerpt" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form
    heroImageClear <- Form.parseMaybe "hero_image_clear" form

    pure
      BlogEditForm
        { befTitle = title,
          befContent = content,
          befExcerpt = emptyToNothing excerpt,
          befStatus = status,
          befTags = parseTags $ fromMaybe "" tags,
          befHeroImage = Nothing,
          befHeroImageClear = heroImageClear == Just ("true" :: Text)
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse comma-separated tags with sanitization
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText
