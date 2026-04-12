module API.Dashboard.Store.Orders.Id.Notes.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form data for updating order notes.
newtype NotesForm = NotesForm
  { nfNotes :: Text
  }
  deriving (Show)

instance FromForm NotesForm where
  fromForm :: Form.Form -> Either Text NotesForm
  fromForm form = do
    notes <- Form.parseUnique "notes" form
    pure NotesForm {nfNotes = notes}

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/orders/:id/notes"
type Route =
  "dashboard"
    :> "store"
    :> "orders"
    :> Servant.Capture "id" Orders.Id
    :> "notes"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] NotesForm
    :> Servant.Post '[HTML] (Lucid.Html ())
