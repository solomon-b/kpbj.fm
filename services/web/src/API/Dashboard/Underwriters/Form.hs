-- | The form both underwriter POST routes take.
module API.Dashboard.Underwriters.Form
  ( UnderwriterForm (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form data for adding or renaming an underwriter.
newtype UnderwriterForm = UnderwriterForm
  { uwfName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Form.FromForm UnderwriterForm where
  fromForm form = UnderwriterForm <$> Form.parseUnique "name" form
