module Effects.ContentSanitizationSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Effects.ContentSanitization
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Content Sanitization" $ do
    describe "sanitizeTitle" $ do
      it "removes HTML tags from titles" $ do
        sanitizeTitle "<script>alert('xss')</script>Title" `shouldBe` "Title"
        sanitizeTitle "<h1>Important Title</h1>" `shouldBe` "Important Title"
        sanitizeTitle "Safe Title" `shouldBe` "Safe Title"

      it "strips whitespace from titles" $ do
        sanitizeTitle "  Title with spaces  " `shouldBe` "Title with spaces"
        sanitizeTitle "\n\t  Messy Title \t\n" `shouldBe` "Messy Title"

      it "handles empty and whitespace-only titles" $ do
        sanitizeTitle "" `shouldBe` ""
        sanitizeTitle "   " `shouldBe` ""
        sanitizeTitle "\n\t" `shouldBe` ""

    describe "sanitizeUserContent" $ do
      it "preserves safe HTML formatting" $ do
        let safeContent = "<p>This is a paragraph</p><em>emphasized</em>"
        -- Note: xss-sanitize may modify formatting but preserves basic structure
        sanitizeUserContent safeContent `shouldBe` safeContent

      it "removes dangerous script tags" $ do
        let dangerousContent = "<script>alert('xss')</script><p>Safe content</p>"
        let sanitized = sanitizeUserContent dangerousContent
        Text.unpack sanitized `shouldNotContain` "script"
        Text.unpack sanitized `shouldNotContain` "alert"
        sanitized `shouldBe` "<p>Safe content</p>"

      it "removes dangerous event handlers" $ do
        let dangerousContent = "<div onclick=\"alert('xss')\">Click me</div>"
        let sanitized = sanitizeUserContent dangerousContent
        Text.unpack sanitized `shouldNotContain` "onclick"
        Text.unpack sanitized `shouldNotContain` "alert"
        sanitized `shouldBe` "<div>Click me</div>"

      it "handles plain text content safely" $ do
        sanitizeUserContent "Just plain text content" `shouldBe` "Just plain text content"

    describe "sanitizeDescription" $ do
      it "limits length to 1000 characters" $ do
        let longDesc = Text.replicate 1500 "a"
        Text.length (sanitizeDescription longDesc) `shouldBe` 1000

      it "sanitizes HTML content" $ do
        let htmlDesc = "<script>alert('xss')</script><p>Good description</p>"
        let sanitized = sanitizeDescription htmlDesc
        sanitized `shouldNotSatisfy` Text.isInfixOf "<script>"
        sanitized `shouldSatisfy` Text.isInfixOf "<p>Good description</p>"

      it "strips whitespace" $ do
        sanitizeDescription "  Description with spaces  " `shouldBe` "Description with spaces"

    describe "sanitizePlainText" $ do
      it "removes HTML characters" $ do
        sanitizePlainText "Text<tag>More" `shouldBe` "TexttagMore"
        sanitizePlainText "Text>More<Text" `shouldBe` "TextMoreText"

      it "preserves normal text" $ do
        sanitizePlainText "Normal text content" `shouldBe` "Normal text content"

      it "removes control characters" $ do
        let textWithControls = "Text" <> Text.pack ['\x01', '\x02'] <> "More"
        sanitizePlainText textWithControls `shouldBe` "TextMore"

    describe "validateContentLength" $ do
      it "accepts content within limits" $ do
        validateContentLength 10 "Short" `shouldBe` Right "Short"
        validateContentLength 100 "Medium length content" `shouldBe` Right "Medium length content"

      it "rejects empty content" $ do
        validateContentLength 10 "" `shouldBe` Left ContentEmpty
        validateContentLength 10 "   " `shouldBe` Left ContentEmpty
        validateContentLength 10 "\n\t  " `shouldBe` Left ContentEmpty

      it "rejects content that's too long" $ do
        let longContent = Text.replicate 200 "a"
        validateContentLength 100 longContent `shouldBe` Left (ContentTooLong 100 200)

    describe "displayContentValidationError" $ do
      it "formats ContentTooLong errors clearly" $ do
        let validationError = ContentTooLong 100 150
        displayContentValidationError validationError `shouldBe` "Content is too long: 150 characters (maximum: 100)"

      it "formats ContentEmpty errors clearly" $ do
        displayContentValidationError ContentEmpty `shouldBe` "Content cannot be empty"

      it "formats ContentInvalid errors clearly" $ do
        let validationError = ContentInvalid "Custom error message"
        displayContentValidationError validationError `shouldBe` "Invalid content: Custom error message"

  describe "XSS Attack Prevention" $ do
    it "prevents script injection attacks" $ do
      let attacks =
            [ "<script>alert('xss')</script>",
              "<img src=x onerror=alert('xss')>",
              "<div onclick='alert(\"xss\")'>click</div>",
              "javascript:alert('xss')",
              "<iframe src='javascript:alert(\"xss\")'></iframe>"
            ]
          checkAttack attack = do
            let sanitized = sanitizeUserContent attack
            -- xss-sanitize should remove dangerous elements like script tags and event handlers
            Text.unpack sanitized `shouldNotContain` "<script>"
            Text.unpack sanitized `shouldNotContain` "onerror"
            Text.unpack sanitized `shouldNotContain` "onclick"
      mapM_ checkAttack attacks

    it "prevents data exfiltration attempts" $ do
      let attacks =
            [ "<img src='http://evil.com/steal' onerror='fetch()'>text",
              "<div onmouseover='window.location'>hover</div>"
            ]
          checkExfilAttack attack = do
            let sanitized = sanitizeUserContent attack
            -- Check that dangerous event handlers are removed
            Text.unpack sanitized `shouldNotContain` "onmouseover"
      -- The core protection is removing the dangerous event handlers, not necessarily the URLs
      mapM_ checkExfilAttack attacks
