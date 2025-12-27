module API.Dashboard.StationBlog.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug ()
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FileData,
    FromMultipart,
    Mem,
    MultipartForm,
    fdFileName,
    fromMultipart,
    lookupFile,
    lookupInput,
  )
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- | Form data for creating a new blog post
data NewBlogPostForm = NewBlogPostForm
  { nbpfTitle :: Text,
    nbpfContent :: Text,
    nbpfExcerpt :: Maybe Text,
    nbpfStatus :: Maybe Text,
    nbpfTags :: [Text],
    nbpfHeroImage :: Maybe (FileData Mem)
  }
  deriving (Show, Eq)

instance FromMultipart Mem NewBlogPostForm where
  fromMultipart multipartData =
    NewBlogPostForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "content" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "excerpt" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "status" multipartData))
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "hero_image" multipartData))
    where
      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

instance FromForm NewBlogPostForm where
  fromForm form = do
    title <- parseUnique "title" form
    content <- parseUnique "content" form
    excerpt <- parseMaybe "excerpt" form
    status <- parseMaybe "status" form
    tags <- parseMaybe "tags" form

    pure
      NewBlogPostForm
        { nbpfTitle = title,
          nbpfContent = content,
          nbpfExcerpt = if maybe True Text.null excerpt then Nothing else excerpt,
          nbpfStatus = status,
          nbpfTags = parseTags $ fold tags,
          nbpfHeroImage = Nothing
        }

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map Text.strip $
      Text.splitOn "," tagText

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/station-blog/new"
    ( "dashboard"
        :> "station-blog"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem NewBlogPostForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )
