{-# LANGUAGE TemplateHaskell #-}

-- | CSS styles for the form builder semantic classes.
--
-- This module provides CSS rules that map the semantic fb-* classes
-- from lucid-form-builder to actual styles. The styles are theme-aware
-- using CSS custom properties.
module Design.FormStyles (formBuilderCSS) where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

--------------------------------------------------------------------------------

-- | CSS rules for form builder semantic classes.
--
-- Include this in your page's <style> tag.
formBuilderCSS :: Text
formBuilderCSS = decodeUtf8 formBuilderCSSBytes

-- | Raw CSS bytes embedded from static/forms.css
formBuilderCSSBytes :: ByteString
formBuilderCSSBytes = $(embedFile "static/forms.css")
