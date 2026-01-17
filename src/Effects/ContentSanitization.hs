{-# LANGUAGE OverloadedStrings #-}

-- | Content sanitization for user-generated content to prevent XSS attacks
--
-- This module provides sanitization functions for user-generated text content
-- that may contain HTML. It uses the xss-sanitize library to allow safe HTML
-- formatting while removing potentially malicious content.
--
-- The sanitization strategy allows basic formatting (paragraphs, emphasis, links)
-- while removing script tags, dangerous attributes, and other XSS vectors.
module Effects.ContentSanitization
  ( -- * Content Sanitization
    sanitizeUserContent,
    sanitizeTitle,
    sanitizeDisplayName,
    sanitizeDescription,
    sanitizePlainText,

    -- * Validation
    validateContentLength,
    ContentValidationError (..),
    displayContentValidationError,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Text.HTML.SanitizeXSS (sanitizeXSS)

--------------------------------------------------------------------------------
-- Content Validation

-- | Errors that can occur during content validation
data ContentValidationError
  = -- | Maximum length, actual length
    ContentTooLong Int Int
  | ContentEmpty
  | -- | Error message
    ContentInvalid Text
  deriving stock (Show, Eq)

-- | Validate content length
validateContentLength :: Int -> Text -> Either ContentValidationError Text
validateContentLength maxLen content
  | Text.null (Text.strip content) = Left ContentEmpty
  | Text.length content > maxLen = Left $ ContentTooLong maxLen (Text.length content)
  | otherwise = Right content

-- | Convert ContentValidationError to user-friendly display text
displayContentValidationError :: ContentValidationError -> Text
displayContentValidationError = \case
  ContentTooLong maxLen actualLen ->
    "Content is too long: " <> Text.pack (show actualLen) <> " characters (maximum: " <> Text.pack (show maxLen) <> ")"
  ContentEmpty ->
    "Content cannot be empty"
  ContentInvalid msg ->
    "Invalid content: " <> msg

--------------------------------------------------------------------------------
-- Content Sanitization

-- | Sanitize user-generated content that may contain HTML formatting
--
-- This function allows basic HTML formatting (paragraphs, emphasis, links)
-- while removing potentially dangerous content like scripts, dangerous attributes,
-- and malicious HTML elements.
--
-- Use this for:
-- - Blog post content
-- - Episode descriptions
-- - Event descriptions
-- - Any content where users should be able to include basic HTML formatting
sanitizeUserContent :: Text -> Text
sanitizeUserContent = sanitizeXSS

-- | Sanitize titles and headings
--
-- This removes all HTML tags since titles should be plain text.
-- Use this for:
-- - Blog post titles
-- - Episode titles
-- - Event titles
-- - Show titles
sanitizeTitle :: Text -> Text
sanitizeTitle title =
  -- First sanitize to remove dangerous content, then strip all remaining HTML
  Text.strip $ stripAllHTML $ sanitizeXSS title
  where
    -- Remove HTML tags properly using regex-like replacement
    stripAllHTML text =
      let -- Simple state machine to remove everything between < and >
          go [] acc _ = acc
          go (c : cs) acc inTag
            | c == '<' = go cs acc True
            | c == '>' = go cs acc False
            | inTag = go cs acc True
            | otherwise = go cs (c : acc) False
       in Text.pack $ reverse $ go (Text.unpack text) [] False

-- | Sanitize display names and user names
--
-- This removes HTML and limits to plain text with basic character validation.
-- Use this for:
-- - User display names
-- - Author names
-- - Host names
sanitizeDisplayName :: Text -> Text
sanitizeDisplayName name =
  Text.strip $ Text.filter isValidNameChar $ Text.take 100 name
  where
    isValidNameChar c = c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c >= ' '

-- | Sanitize description fields
--
-- This allows basic HTML formatting but with stricter limits than full content.
-- Use this for:
-- - Short descriptions
-- - Excerpts
-- - Location descriptions
sanitizeDescription :: Text -> Text
sanitizeDescription desc =
  Text.strip $ sanitizeXSS $ Text.take 1000 desc

-- | Sanitize plain text fields
--
-- This removes all HTML and provides basic text validation.
-- Use this for:
-- - Tags
-- - Labels
-- - Simple text fields
sanitizePlainText :: Text -> Text
sanitizePlainText plainText =
  Text.strip $ Text.filter isValidTextChar plainText
  where
    isValidTextChar c = c /= '<' && c /= '>' && c >= ' '
