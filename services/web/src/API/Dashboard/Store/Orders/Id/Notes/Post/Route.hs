module API.Dashboard.Store.Orders.Id.Notes.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
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
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.ReqBody '[Servant.FormUrlEncoded] NotesForm
    :> Servant.Post '[HTML] (Lucid.Html ())
