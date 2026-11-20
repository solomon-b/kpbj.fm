-- | Parsers for form fields.
--
-- A form decoder calls these, so the handler receives a typed value and does
-- no parse of its own. The 'Left' text is for the user. "App.ErrorFormatters"
-- shows it in a banner.
module App.FormField
  ( emailField,
  )
where

--------------------------------------------------------------------------------

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress (EmailAddress, mkEmailAddress)

--------------------------------------------------------------------------------

-- | Parse the text of an email field.
--
-- Spaces around the address are removed first. A user often pastes them.
emailField :: Text -> Either Text EmailAddress
emailField =
  first (const "Please enter a valid email address.")
    . mkEmailAddress
    . Text.strip
