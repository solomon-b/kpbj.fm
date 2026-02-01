module Domain.Types.UserSortBy
  ( UserSortBy (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, displayBuilder)
import Servant qualified

--------------------------------------------------------------------------------

data UserSortBy
  = JoinDateNewest
  | JoinDateOldest
  | NameAZ
  | ShowCount
  | StatusSuspended
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Display UserSortBy where
  displayBuilder JoinDateNewest = "newest"
  displayBuilder JoinDateOldest = "oldest"
  displayBuilder NameAZ = "name"
  displayBuilder ShowCount = "shows"
  displayBuilder StatusSuspended = "status"

instance Servant.FromHttpApiData UserSortBy where
  parseUrlPiece "newest" = Right JoinDateNewest
  parseUrlPiece "oldest" = Right JoinDateOldest
  parseUrlPiece "name" = Right NameAZ
  parseUrlPiece "shows" = Right ShowCount
  parseUrlPiece "status" = Right StatusSuspended
  parseUrlPiece invalid = Left $ "Invalid sort: " <> invalid

instance Servant.ToHttpApiData UserSortBy where
  toUrlPiece JoinDateNewest = "newest"
  toUrlPiece JoinDateOldest = "oldest"
  toUrlPiece NameAZ = "name"
  toUrlPiece ShowCount = "shows"
  toUrlPiece StatusSuspended = "status"
