module Middleware.FlashSpec (spec) where

--------------------------------------------------------------------------------

import Component.Flash (clearFlashCookie)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text.Encoding qualified as Text.Encoding
import Middleware.Flash (flashMiddleware)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Header qualified as Header
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Middleware.Flash" $ do
  describe "flashMiddleware" $ do
    it "deletes the cookie after a full page" $ do
      cookies <- setCookies [flashRequestCookie] HTTP.status200 [htmlType]
      cookies `shouldBe` [clearFlash]

    it "deletes the cookie after an HTML error page" $ do
      cookies <- setCookies [flashRequestCookie] HTTP.status404 [htmlType]
      cookies `shouldBe` [clearFlash]

    it "keeps the cookie after an HTMX fragment" $ do
      cookies <- setCookies [flashRequestCookie, ("HX-Request", "true")] HTTP.status200 [htmlType]
      cookies `shouldBe` []

    it "keeps the cookie after a response that is not HTML" $ do
      cookies <- setCookies [flashRequestCookie] HTTP.status200 [(Header.hContentType, "application/json")]
      cookies `shouldBe` []

    it "adds nothing to a request with no flash cookie" $ do
      cookies <- setCookies [(Header.hCookie, "session=abc")] HTTP.status200 [htmlType]
      cookies `shouldBe` []

    it "keeps the cookie after a redirect with no message" $ do
      cookies <- setCookies [flashRequestCookie] HTTP.status200 [htmlType, ("HX-Redirect", "/dashboard/episodes")]
      cookies `shouldBe` []

    it "deletes the cookie after a full page for HX-Request: false" $ do
      cookies <- setCookies [flashRequestCookie, ("HX-Request", "false")] HTTP.status200 [htmlType]
      cookies `shouldBe` [clearFlash]

    it "keeps the new cookie of a redirect with a new message" $ do
      let newFlash = "_flash=new; Path=/; HttpOnly; SameSite=Strict"
      cookies <- setCookies [flashRequestCookie] HTTP.status200 [htmlType, (Header.hSetCookie, newFlash)]
      cookies `shouldBe` [newFlash]

--------------------------------------------------------------------------------

-- | A @Cookie@ request header with a flash cookie.
flashRequestCookie :: Header.Header
flashRequestCookie = (Header.hCookie, "session=abc; _flash=%7B%7D")

-- | The content type of a page.
htmlType :: Header.Header
htmlType = (Header.hContentType, "text/html;charset=utf-8")

-- | The @Set-Cookie@ value that deletes the flash cookie.
clearFlash :: ByteString
clearFlash = Text.Encoding.encodeUtf8 clearFlashCookie

-- | Send a request with the headers through the middleware, to an app that
-- answers with the status and the headers. Return the @Set-Cookie@ values
-- of the response.
setCookies :: HTTP.RequestHeaders -> HTTP.Status -> HTTP.ResponseHeaders -> IO [ByteString]
setCookies requestHeaders status responseHeaders = do
  headersRef <- newIORef []
  let request = Wai.defaultRequest {Wai.requestHeaders = requestHeaders}
      app _req respond = respond $ Wai.responseLBS status responseHeaders ""
  _ <- flashMiddleware app request $ \response -> do
    writeIORef headersRef (Wai.responseHeaders response)
    pure ResponseReceived
  headers <- readIORef headersRef
  pure [value | (name, value) <- headers, name == Header.hSetCookie]
