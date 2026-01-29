{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Markdown rendering for user-generated content with Tailwind CSS styling.
--
-- This module provides markdown-to-HTML conversion with automatic Tailwind CSS
-- class application. It supports common markdown features including headings,
-- paragraphs, code blocks, lists, links, emphasis, blockquotes, and images.
--
-- The rendering pipeline:
--
--   1. Parse markdown text using Pandoc
--   2. Convert Pandoc AST to HTML5 string
--   3. Parse HTML into xmlhtml nodes
--   4. Apply Tailwind CSS classes using optics
--   5. Render back to Lucid HTML
--
-- Example usage:
--
-- @
-- renderMarkdown defaultMarkdownConfig "# Hello World\n\nThis is **bold** text."
-- @
module Effects.Markdown
  ( -- * Markdown Rendering
    renderMarkdown,
    renderMarkdownPure,

    -- * Content Rendering
    renderContentM,

    -- * Error Types
    MarkdownError (..),
    displayMarkdownError,

    -- * Configuration
    MarkdownConfig (..),
    defaultMarkdownConfig,

    -- * Low-level API
    parseMarkdownToHtml,
    applyTailwindStyles,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (over, set, traversed, (&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Log qualified
import Lucid qualified
import Text.HTML (renderNodes)
import Text.Pandoc
  ( Extension (..),
    def,
    extensionsFromList,
    readMarkdown,
    readerExtensions,
    runPure,
    writeHtml5String,
    writerExtensions,
  )
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------
-- Error Types

-- | Errors that can occur during markdown rendering.
data MarkdownError
  = -- | Pandoc failed to parse the markdown input
    PandocParseError Text
  | -- | xmlhtml failed to parse the generated HTML
    HtmlParseError Text
  deriving stock (Show, Eq)

-- | Convert a MarkdownError to user-friendly display text.
displayMarkdownError :: MarkdownError -> Text
displayMarkdownError = \case
  PandocParseError err ->
    "Failed to parse markdown: " <> err
  HtmlParseError err ->
    "Failed to process HTML output: " <> err

--------------------------------------------------------------------------------
-- Configuration

-- | Configuration for markdown rendering.
data MarkdownConfig = MarkdownConfig
  { -- | Enable GitHub-style task lists
    mcTaskLists :: Bool,
    -- | Enable backtick code blocks
    mcCodeBlocks :: Bool,
    -- | Enable hard line breaks (newlines become <br>)
    mcHardLineBreaks :: Bool,
    -- | Enable strikethrough text (~~text~~)
    mcStrikethrough :: Bool,
    -- | Enable tables
    mcTables :: Bool,
    -- | Enable footnotes
    mcFootnotes :: Bool
  }
  deriving stock (Show, Eq)

-- | Default markdown configuration with commonly used features.
defaultMarkdownConfig :: MarkdownConfig
defaultMarkdownConfig =
  MarkdownConfig
    { mcTaskLists = True,
      mcCodeBlocks = True,
      mcHardLineBreaks = True,
      mcStrikethrough = True,
      mcTables = True,
      mcFootnotes = False
    }

--------------------------------------------------------------------------------
-- Core Rendering Functions

-- | Render markdown text to styled Lucid HTML with logging.
--
-- This is the primary function for converting markdown content to HTML
-- with Tailwind CSS styling applied. It handles errors gracefully by
-- logging them and returning a fallback error display.
renderMarkdown ::
  (Log.MonadLog m) =>
  MarkdownConfig ->
  Text ->
  m (Lucid.Html ())
renderMarkdown config markdown = do
  Log.logTrace "Markdown input" markdown
  case renderMarkdownPure config markdown of
    Left err -> do
      Log.logAttention "Markdown parsing failed" (displayMarkdownError err)
      pure $ renderMarkdownError err
    Right html -> do
      Log.logTrace "Markdown rendered successfully" ()
      pure html

-- | Pure version of markdown rendering for use in templates.
--
-- Returns @Either MarkdownError (Lucid.Html ())@. Useful when you want
-- to handle errors explicitly in template code.
renderMarkdownPure ::
  MarkdownConfig ->
  Text ->
  Either MarkdownError (Lucid.Html ())
renderMarkdownPure config markdown = do
  htmlText <- parseMarkdownToHtml config markdown
  nodes <- parseHtmlFragment htmlText
  pure $ renderNodes (applyTailwindStyles nodes)

--------------------------------------------------------------------------------
-- Low-level API

-- | Parse markdown to HTML string using Pandoc (pure).
parseMarkdownToHtml ::
  MarkdownConfig ->
  Text ->
  Either MarkdownError Text
parseMarkdownToHtml config md =
  case runPure pandocAction of
    Left err -> Left $ PandocParseError $ Text.pack $ show err
    Right html -> Right html
  where
    extensions = configToExtensions config
    readerConfig = def {readerExtensions = extensionsFromList extensions}
    writerConfig = def {writerExtensions = extensionsFromList extensions}
    pandocAction = do
      pandocAst <- readMarkdown readerConfig md
      writeHtml5String writerConfig pandocAst

-- | Parse HTML text into xmlhtml nodes (pure version).
parseHtmlFragment ::
  Text ->
  Either MarkdownError [Xml.Node]
parseHtmlFragment htmlText =
  case Xml.parseHTML "markdown-output.html" (TE.encodeUtf8 htmlText) of
    Left err -> Left $ HtmlParseError $ Text.pack err
    Right doc -> Right $ Xml.docContent doc

-- | Convert configuration to Pandoc extensions.
configToExtensions :: MarkdownConfig -> [Extension]
configToExtensions MarkdownConfig {..} =
  concat
    [ [Ext_backtick_code_blocks | mcCodeBlocks],
      [Ext_task_lists | mcTaskLists],
      [Ext_hard_line_breaks | mcHardLineBreaks],
      [Ext_strikeout | mcStrikethrough],
      [Ext_pipe_tables | mcTables],
      [Ext_footnotes | mcFootnotes]
    ]

--------------------------------------------------------------------------------
-- Tailwind Style Application

-- | Apply Tailwind CSS classes to HTML nodes.
--
-- This function traverses the HTML node tree and adds appropriate
-- Tailwind classes to each element type for consistent styling.
applyTailwindStyles :: [Xml.Node] -> [Xml.Node]
applyTailwindStyles nodes =
  nodes
    -- Headings
    & set (traversed . _el "h1" . _elAttributes) [("class", "text-4xl font-bold mt-8 mb-4 text-[var(--theme-fg)]")]
    & set (traversed . _el "h2" . _elAttributes) [("class", "text-3xl font-bold mt-6 mb-3 text-[var(--theme-fg)]")]
    & set (traversed . _el "h3" . _elAttributes) [("class", "text-2xl font-bold mt-5 mb-2 text-[var(--theme-fg)]")]
    & set (traversed . _el "h4" . _elAttributes) [("class", "text-xl font-bold mt-4 mb-2 text-[var(--theme-fg)]")]
    & set (traversed . _el "h5" . _elAttributes) [("class", "text-lg font-bold mt-3 mb-2 text-[var(--theme-fg-muted)]")]
    & set (traversed . _el "h6" . _elAttributes) [("class", "text-base font-bold mt-2 mb-1 text-[var(--theme-fg-muted)]")]
    -- Paragraphs
    & set (traversed . _el "p" . _elAttributes) [("class", "my-4 text-[var(--theme-fg)] leading-relaxed")]
    -- Lists
    & set (traversed . _el "ul" . _elAttributes) [("class", "list-disc list-inside my-4 ml-4 space-y-2 text-[var(--theme-fg)]")]
    & set (traversed . _el "ol" . _elAttributes) [("class", "list-decimal list-inside my-4 ml-4 space-y-2 text-[var(--theme-fg)]")]
    & set (traversed . _el "li" . _elAttributes) [("class", "leading-relaxed")]
    -- Task list checkboxes
    & over (traversed . _el "ul" . _Node . _el "li" . _Node . _el "input" . _elAttributes) (<> [("class", "mr-2")])
    -- Blockquotes
    & set (traversed . _el "blockquote" . _elAttributes) [("class", "border-l-4 border-[var(--theme-border-muted)] pl-4 my-4 italic text-[var(--theme-fg-muted)]")]
    -- Code blocks
    & set (traversed . _el "pre" . _elAttributes) [("class", "bg-[var(--theme-bg-alt)] border-2 border-[var(--theme-border-muted)] rounded-md p-4 my-4 overflow-x-auto font-mono text-sm text-[var(--theme-fg)]")]
    -- Inline code (inside paragraphs)
    & over (traversed . _el "p" . _Node . _el "code" . _elAttributes) (<> [("class", "bg-[var(--theme-bg-alt)] px-1.5 py-0.5 rounded font-mono text-sm text-[var(--theme-fg)]")])
    -- Code inside pre blocks
    & set (traversed . _el "pre" . _Node . _el "code" . _elAttributes) [("class", "font-mono text-sm")]
    -- Links
    & over (traversed . _el "a" . _elAttributes) (<> [("class", "text-[var(--theme-info)] hover:text-[var(--theme-accent-hover)] hover:underline")])
    -- Images
    & over (traversed . _el "img" . _elAttributes) (<> [("class", "max-w-full h-auto my-4 border-2 border-[var(--theme-border-muted)]")])
    -- Tables
    & set (traversed . _el "table" . _elAttributes) [("class", "min-w-full border-collapse border-2 border-[var(--theme-border-muted)] my-4")]
    & set (traversed . _el "thead" . _elAttributes) [("class", "bg-[var(--theme-bg-alt)]")]
    & set (traversed . _el "th" . _elAttributes) [("class", "border border-[var(--theme-border-muted)] px-4 py-2 text-left font-bold text-[var(--theme-fg)]")]
    & set (traversed . _el "td" . _elAttributes) [("class", "border border-[var(--theme-border-muted)] px-4 py-2 text-[var(--theme-fg)]")]
    -- Horizontal rules
    & set (traversed . _el "hr" . _elAttributes) [("class", "my-8 border-t-2 border-[var(--theme-border-muted)]")]
    -- Strong and emphasis
    & set (traversed . _el "strong" . _elAttributes) [("class", "font-bold")]
    & set (traversed . _el "em" . _elAttributes) [("class", "italic")]
    -- Strikethrough
    & set (traversed . _el "del" . _elAttributes) [("class", "line-through text-gray-500")]

--------------------------------------------------------------------------------
-- Content Rendering

-- | Render content as markdown with Tailwind CSS styling and error logging.
--
-- Parses markdown content, applies Tailwind classes, and logs any parsing
-- errors via 'Log.MonadLog'. On error, displays an error message to the user.
renderContentM :: (Log.MonadLog m) => Text -> m (Lucid.Html ())
renderContentM content = do
  html <- renderMarkdown defaultMarkdownConfig content
  pure $ Lucid.div_ [Lucid.class_ "prose max-w-none"] html

--------------------------------------------------------------------------------
-- Error Display

-- | Render a markdown error as HTML for display to users.
renderMarkdownError :: MarkdownError -> Lucid.Html ()
renderMarkdownError err =
  Lucid.div_ [Lucid.class_ "bg-red-50 border-2 border-red-400 p-4 my-4"] do
    Lucid.p_ [Lucid.class_ "text-red-700 font-bold"] "Content could not be rendered"
    Lucid.p_ [Lucid.class_ "text-red-600 text-sm mt-2"] $
      Lucid.toHtml (displayMarkdownError err)
