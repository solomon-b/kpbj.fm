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
    sanitizeDescription,
    sanitizePlainText,

    -- * Validation
    validateContentLength,
    ContentValidationError (..),
    displayContentValidationError,
    validateOptionalHttpUrl,

    -- * Numeric Parsing
    parseIntDefault0,
    parseNonNegativeInt,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI qualified as URI
import Text.HTML.SanitizeXSS (sanitizeXSS)
import Text.Read (readMaybe)

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

-- | Validate an optional external URL.
--
-- Blank input (after trimming) yields @Right Nothing@ (no link). Otherwise the
-- value must be an absolute @http:@ or @https:@ URL with a non-empty host and at
-- most @maxLen@ characters. The trimmed URL is returned on success.
validateOptionalHttpUrl :: Int -> Text -> Either ContentValidationError (Maybe Text)
validateOptionalHttpUrl maxLen raw =
  let trimmed = Text.strip raw
   in if Text.null trimmed
        then Right Nothing
        else
          if Text.length trimmed > maxLen
            then Left (ContentTooLong maxLen (Text.length trimmed))
            else case URI.parseAbsoluteURI (Text.unpack trimmed) of
              Just uri
                | URI.uriScheme uri `elem` ["http:", "https:"],
                  Just auth <- URI.uriAuthority uri,
                  not (null (URI.uriRegName auth)) ->
                    Right (Just trimmed)
              _ -> Left (ContentInvalid "must be a valid http:// or https:// URL")

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
    -- Strip everything between '<' and '>' using a strict left fold
    stripAllHTML =
      snd . Text.foldl' step (False, Text.empty)
      where
        step (_, acc) '<' = (True, acc)
        step (True, acc) '>' = (False, acc)
        step (True, acc) _ = (True, acc)
        step (False, acc) c = (False, Text.snoc acc c)

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

--------------------------------------------------------------------------------
-- Numeric Parsing

-- | Parse a non-negative integer from text, defaulting to 0 on failure.
--
-- Use for fields where 0 is a safe default (weight, sort order).
-- For fields where silent zeroing is dangerous (inventory), use 'parseNonNegativeInt'.
parseIntDefault0 :: Text -> Int64
parseIntDefault0 txt =
  case readMaybe (Text.unpack txt) :: Maybe Int64 of
    Just n | n >= 0 -> n
    _ -> 0

-- | Parse a non-negative integer from text, returning an error on failure.
--
-- Use for fields where the value matters and silent defaulting would be data loss
-- (e.g., inventory count).
parseNonNegativeInt :: Text -> Either Text Int64
parseNonNegativeInt txt =
  case readMaybe (Text.unpack txt) :: Maybe Int64 of
    Just n | n >= 0 -> Right n
    Just _ -> Left "Value must be non-negative"
    Nothing -> Left "Invalid number"

--------------------------------------------------------------------------------
-- Content Sanitization

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
