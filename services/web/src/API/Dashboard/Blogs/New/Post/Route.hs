module API.Dashboard.Blogs.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Slug (Slug)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- | "POST /dashboard/blog/:show_slug/new"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> "new"
    :> RequireRole "cookie-auth" 'Host
    :> Servant.ReqBody '[Servant.FormUrlEncoded] NewShowBlogPostForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)

--------------------------------------------------------------------------------

-- | Form data for creating a new show blog post
data NewShowBlogPostForm = NewShowBlogPostForm
  { nsbpfTitle :: Text,
    nsbpfContent :: Text,
    nsbpfExcerpt :: Maybe Text,
    nsbpfStatus :: Maybe Text,
    nsbpfTags :: [Text]
  }
  deriving (Show, Eq)

instance FromForm NewShowBlogPostForm where
  fromForm form = do
    title <- parseUnique "title" form
    content <- parseUnique "content" form
    excerpt <- parseMaybe "excerpt" form
    status <- parseMaybe "status" form
    tags <- parseMaybe "tags" form

    pure
      NewShowBlogPostForm
        { nsbpfTitle = title,
          nsbpfContent = content,
          nsbpfExcerpt = if maybe True Text.null excerpt then Nothing else excerpt,
          nsbpfStatus = status,
          nsbpfTags = parseTags $ fold tags
        }

-- | Parse comma-separated tags with sanitization
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText
