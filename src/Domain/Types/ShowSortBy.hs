module Domain.Types.ShowSortBy
  ( ShowSortBy (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, displayBuilder)
import Servant qualified

--------------------------------------------------------------------------------

-- | Sort options for the shows listing page.
--
-- Supports sorting by creation date or name, in ascending or descending order.
data ShowSortBy
  = CreatedNewest
  | CreatedOldest
  | NameAZ
  | NameZA
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Display ShowSortBy where
  displayBuilder CreatedNewest = "created_newest"
  displayBuilder CreatedOldest = "created_oldest"
  displayBuilder NameAZ = "name_az"
  displayBuilder NameZA = "name_za"

instance Servant.FromHttpApiData ShowSortBy where
  parseUrlPiece "created_newest" = Right CreatedNewest
  parseUrlPiece "created_oldest" = Right CreatedOldest
  parseUrlPiece "name_az" = Right NameAZ
  parseUrlPiece "name_za" = Right NameZA
  parseUrlPiece invalid = Left $ "Invalid sort: " <> invalid

instance Servant.ToHttpApiData ShowSortBy where
  toUrlPiece CreatedNewest = "created_newest"
  toUrlPiece CreatedOldest = "created_oldest"
  toUrlPiece NameAZ = "name_az"
  toUrlPiece NameZA = "name_za"
