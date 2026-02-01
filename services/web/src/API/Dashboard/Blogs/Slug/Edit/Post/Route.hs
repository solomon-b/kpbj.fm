module API.Dashboard.Blogs.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | "POST /dashboard/blog/:show_slug/:post_id/edit"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "post_id" ShowBlogPosts.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] ShowBlogEditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

-- | Form data for show blog post editing
data ShowBlogEditForm = ShowBlogEditForm
  { sbefTitle :: Text,
    sbefContent :: Text,
    sbefExcerpt :: Maybe Text,
    sbefStatus :: Text,
    sbefTags :: [Text]
  }
  deriving (Show)

instance FromForm ShowBlogEditForm where
  fromForm :: Form.Form -> Either Text ShowBlogEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    content <- Form.parseUnique "content" form
    excerpt <- Form.parseMaybe "excerpt" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form

    pure
      ShowBlogEditForm
        { sbefTitle = title,
          sbefContent = content,
          sbefExcerpt = emptyToNothing excerpt,
          sbefStatus = status,
          sbefTags = parseTags $ fromMaybe "" tags
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map Text.strip $
      Text.splitOn "," tagText
