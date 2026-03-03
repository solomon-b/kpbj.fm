module Domain.Types.PostStatus where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Rel8 (DBEq, DBType (..), TypeInformation, parseTypeInformation, typeInformation)
import Servant qualified

--------------------------------------------------------------------------------

data BlogPostStatus = Draft | Published | Deleted
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance DBType BlogPostStatus where
  typeInformation :: TypeInformation BlogPostStatus
  typeInformation =
    parseTypeInformation
      ( \case
          "draft" -> Right Draft
          "published" -> Right Published
          "deleted" -> Right Deleted
          other -> Left $ "Invalid BlogPostStatus: " <> Text.unpack other
      )
      ( \case
          Draft -> "draft"
          Published -> "published"
          Deleted -> "deleted"
      )
      typeInformation

instance DBEq BlogPostStatus

instance Display BlogPostStatus where
  displayBuilder Draft = "draft"
  displayBuilder Published = "published"
  displayBuilder Deleted = "deleted"

instance DecodeValue BlogPostStatus where
  decodeValue = Decoders.enum decodeBlogPost

decodeBlogPost :: Text -> Maybe BlogPostStatus
decodeBlogPost = \case
  "draft" -> Just Draft
  "published" -> Just Published
  "deleted" -> Just Deleted
  _ -> Nothing

instance EncodeValue BlogPostStatus where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Published -> "published"
    Deleted -> "deleted"

instance Servant.FromHttpApiData BlogPostStatus where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "published" = Right Published
  parseUrlPiece "deleted" = Right Deleted
  parseUrlPiece invalid = Left $ "Invalid BlogPostStatus: " <> invalid

instance Servant.ToHttpApiData BlogPostStatus where
  toUrlPiece = display
