module Effects.MarkdownSpec (spec) where

--------------------------------------------------------------------------------

import Data.Either (isRight)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Effects.Markdown
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Markdown Rendering" $ do
    describe "parseMarkdownToHtml" $ do
      it "converts basic markdown to HTML" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "# Hello World"
        result `shouldSatisfy` isRight
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<h1"
          Left _ -> expectationFailure "Should have parsed"

      it "handles bold text" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "**bold**"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<strong>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles italic text" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "*italic*"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<em>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles links" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "[link](https://example.com)"
        case result of
          Right html -> do
            html `shouldSatisfy` Text.isInfixOf "<a"
            html `shouldSatisfy` Text.isInfixOf "href=\"https://example.com\""
          Left _ -> expectationFailure "Should have parsed"

      it "handles code blocks" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "```\ncode\n```"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<pre"
          Left _ -> expectationFailure "Should have parsed"

      it "handles inline code" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "`inline code`"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<code>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles unordered lists" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "- item 1\n- item 2"
        case result of
          Right html -> do
            html `shouldSatisfy` Text.isInfixOf "<ul>"
            html `shouldSatisfy` Text.isInfixOf "<li>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles ordered lists" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "1. first\n2. second"
        case result of
          Right html -> do
            html `shouldSatisfy` Text.isInfixOf "<ol>"
            html `shouldSatisfy` Text.isInfixOf "<li>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles blockquotes" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "> quoted text"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<blockquote>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles headings at all levels" $ do
        let headings =
              [ ("# H1", "<h1"),
                ("## H2", "<h2"),
                ("### H3", "<h3"),
                ("#### H4", "<h4"),
                ("##### H5", "<h5"),
                ("###### H6", "<h6")
              ]
        forM_ headings $ \(md, expected) -> do
          let result = parseMarkdownToHtml defaultMarkdownConfig md
          case result of
            Right html -> html `shouldSatisfy` Text.isInfixOf expected
            Left _ -> expectationFailure $ "Should have parsed: " <> Text.unpack md

      it "handles horizontal rules" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "---"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<hr"
          Left _ -> expectationFailure "Should have parsed"

      it "handles tables" $ do
        let tableMarkdown =
              "| Header 1 | Header 2 |\n\
              \|----------|----------|\n\
              \| Cell 1   | Cell 2   |"
        let result = parseMarkdownToHtml defaultMarkdownConfig tableMarkdown
        case result of
          Right html -> do
            html `shouldSatisfy` Text.isInfixOf "<table>"
            html `shouldSatisfy` Text.isInfixOf "<th>"
            html `shouldSatisfy` Text.isInfixOf "<td>"
          Left _ -> expectationFailure "Should have parsed"

      it "handles strikethrough" $ do
        let result = parseMarkdownToHtml defaultMarkdownConfig "~~strikethrough~~"
        case result of
          Right html -> html `shouldSatisfy` Text.isInfixOf "<del>"
          Left _ -> expectationFailure "Should have parsed"

    describe "renderMarkdownPure" $ do
      it "applies Tailwind classes to headings" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "# Title"
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "font-bold"
            rendered `shouldSatisfy` LText.isInfixOf "text-4xl"
          Left _ -> expectationFailure "Should have rendered"

      it "applies Tailwind classes to paragraphs" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "A paragraph of text."
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "text-[var(--theme-fg)]"
            rendered `shouldSatisfy` LText.isInfixOf "leading-relaxed"
          Left _ -> expectationFailure "Should have rendered"

      it "applies Tailwind classes to links" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "[link](https://example.com)"
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "text-[var(--theme-info)]"
            rendered `shouldSatisfy` LText.isInfixOf "hover:underline"
          Left _ -> expectationFailure "Should have rendered"

      it "applies Tailwind classes to code blocks" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "```\ncode\n```"
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "bg-[var(--theme-bg-alt)]"
            rendered `shouldSatisfy` LText.isInfixOf "font-mono"
          Left _ -> expectationFailure "Should have rendered"

      it "applies Tailwind classes to blockquotes" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "> quoted text"
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "border-l-4"
            rendered `shouldSatisfy` LText.isInfixOf "italic"
          Left _ -> expectationFailure "Should have rendered"

      it "applies Tailwind classes to lists" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "- item 1\n- item 2"
        case result of
          Right html -> do
            let rendered = Lucid.renderText html
            rendered `shouldSatisfy` LText.isInfixOf "list-disc"
          Left _ -> expectationFailure "Should have rendered"

    describe "Error handling" $ do
      it "handles empty input gracefully" $ do
        let result = renderMarkdownPure defaultMarkdownConfig ""
        result `shouldSatisfy` isRight

      it "handles plain text without markdown" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "Just plain text"
        result `shouldSatisfy` isRight

      it "handles whitespace-only input" $ do
        let result = renderMarkdownPure defaultMarkdownConfig "   \n\n   "
        result `shouldSatisfy` isRight

  describe "XSS Prevention" $ do
    it "escapes raw HTML in markdown" $ do
      -- Pandoc by default does not allow raw HTML in markdown unless explicitly enabled
      let result = parseMarkdownToHtml defaultMarkdownConfig "<script>alert('xss')</script>"
      case result of
        Right html -> do
          -- The script tag should not be rendered as-is
          html `shouldNotSatisfy` Text.isInfixOf "<script>"
        Left _ -> pure () -- If it rejects, that's also safe
    it "escapes dangerous attributes" $ do
      let result = parseMarkdownToHtml defaultMarkdownConfig "<div onclick=\"alert('xss')\">click</div>"
      case result of
        Right html -> do
          -- The div tag should be escaped (< becomes &lt;), making onclick harmless text
          -- Check that it's not a real HTML attribute (would have unescaped < before onclick)
          html `shouldNotSatisfy` Text.isInfixOf "<div onclick"
          -- The angle brackets should be escaped
          html `shouldSatisfy` Text.isInfixOf "&lt;div"
        Left _ -> pure ()

    it "preserves safe markdown formatting" $ do
      let safeMarkdown = "**Bold** and *italic* and [link](https://example.com)"
      let result = renderMarkdownPure defaultMarkdownConfig safeMarkdown
      result `shouldSatisfy` isRight

  describe "displayMarkdownError" $ do
    it "formats PandocParseError clearly" $ do
      let err = PandocParseError "Some parse error"
      displayMarkdownError err `shouldBe` "Failed to parse markdown: Some parse error"

    it "formats HtmlParseError clearly" $ do
      let err = HtmlParseError "Some HTML error"
      displayMarkdownError err `shouldBe` "Failed to process HTML output: Some HTML error"

  describe "MarkdownConfig" $ do
    it "has sensible defaults" $ do
      mcTaskLists defaultMarkdownConfig `shouldBe` True
      mcCodeBlocks defaultMarkdownConfig `shouldBe` True
      mcHardLineBreaks defaultMarkdownConfig `shouldBe` False
      mcStrikethrough defaultMarkdownConfig `shouldBe` True
      mcTables defaultMarkdownConfig `shouldBe` True
      mcFootnotes defaultMarkdownConfig `shouldBe` False
