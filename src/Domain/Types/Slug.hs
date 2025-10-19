module Domain.Types.Slug where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

newtype Slug = Slug Text
  deriving stock (Show)
  deriving newtype (Eq, Display, Servant.ToHttpApiData, Servant.FromHttpApiData, IsString, EncodeValue, DecodeValue, FromJSON, ToJSON)

instance Semigroup Slug where
  Slug x <> Slug "" = Slug x
  Slug "" <> Slug y = Slug y
  Slug x <> Slug y = Slug $ x <> "-" <> y

--------------------------------------------------------------------------------

-- | Generate a URL-friendly slug from text.
-- Filters to ASCII alphanumerics and hyphens, replaces spaces with hyphens,
-- and converts to lowercase.
--
-- Examples:
--   mkSlug "Hello World" == Slug "hello-world"
--   mkSlug "My Show!" == Slug "my-show"
--   mkSlug "Test 123" == Slug "test-123"
mkSlug :: Text -> Slug
mkSlug title =
  Slug $
    Text.toLower $
      Text.replace " " "-" $
        Text.filter (\c -> c `elem` ("-" :: String) || isAsciiLower c || isAsciiUpper c || isDigit c) title
