module Domain.Types.PostStatus where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Servant qualified

--------------------------------------------------------------------------------

data BlogPostStatus = Draft | Published | Archived
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display BlogPostStatus where
  displayBuilder Draft = "draft"
  displayBuilder Published = "published"
  displayBuilder Archived = "archived"

instance DecodeValue BlogPostStatus where
  decodeValue = Decoders.enum decodeBlogPost

decodeBlogPost :: Text -> Maybe BlogPostStatus
decodeBlogPost = \case
  "draft" -> Just Draft
  "published" -> Just Published
  "archived" -> Just Archived
  _ -> Nothing

instance EncodeValue BlogPostStatus where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Published -> "published"
    Archived -> "archived"

instance Servant.FromHttpApiData BlogPostStatus where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "published" = Right Published
  parseUrlPiece "archived" = Right Archived
  parseUrlPiece invalid = Left $ "Invalid BlogPostStatus: " <> invalid

instance Servant.ToHttpApiData BlogPostStatus where
  toUrlPiece = display
