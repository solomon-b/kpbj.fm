{-# LANGUAGE OverloadedStrings #-}

-- | Shared hint text for form fields.
--
-- Each prose text area uses the same words, so the site tells authors the
-- same thing on every form.
module Component.Form.Hints
  ( markdownHint,
    plainTextHint,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Hint for a text area that the site renders as Markdown.
markdownHint :: Text
markdownHint = "Markdown supported."

-- | Hint for a text area that the site shows as plain text.
--
-- Excerpts appear in summaries, and summaries remove Markdown formatting.
plainTextHint :: Text
plainTextHint = "Shown as plain text. Markdown formatting is removed."
