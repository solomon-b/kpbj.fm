module Middleware.ValidateEncodingSpec (spec) where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text.Encoding qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Middleware.ValidateEncoding (isValidUtf8, validateEncodingMiddleware)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Middleware.ValidateEncoding" $ do
  describe "isValidUtf8" $ do
    it "accepts an empty bytestring" $
      isValidUtf8 "" `shouldBe` True

    it "accepts a valid ASCII path" $
      isValidUtf8 "/api/shows/123" `shouldBe` True

    it "accepts valid multi-byte UTF-8" $
      -- é encoded as %C3%A9
      isValidUtf8 "/caf%C3%A9" `shouldBe` True

    it "rejects lone \\xAD (the exploit payload)" $
      isValidUtf8 "%AD" `shouldBe` False

    it "rejects \\xFF" $
      isValidUtf8 "%FF" `shouldBe` False

    it "rejects \\xFE" $
      isValidUtf8 "%FE" `shouldBe` False

    it "rejects \\xC0 (overlong encoding start)" $
      isValidUtf8 "%C0" `shouldBe` False

    it "rejects invalid byte embedded in valid text" $
      isValidUtf8 "/valid%ADpath" `shouldBe` False

    it "accepts any valid Text after UTF-8 percent-encoding" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
      assert $ isValidUtf8 (HTTP.urlEncode False (Text.encodeUtf8 t))

  describe "validateEncodingMiddleware" $ do
    it "passes through a request with an ASCII path" $ do
      (status, passed) <- runMiddleware "/hello" ""
      passed `shouldBe` True
      status `shouldBe` 200

    it "passes through percent-encoded valid UTF-8 in path" $ do
      (status, passed) <- runMiddleware "/caf%C3%A9" ""
      passed `shouldBe` True
      status `shouldBe` 200

    it "blocks %AD in path with 400" $ do
      (status, passed) <- runMiddleware "/exploit%AD" ""
      passed `shouldBe` False
      status `shouldBe` 400

    it "blocks %AD in query string with 400" $ do
      (status, passed) <- runMiddleware "/" "?q=%AD"
      passed `shouldBe` False
      status `shouldBe` 400

    it "passes through empty path and query" $ do
      (status, passed) <- runMiddleware "" ""
      passed `shouldBe` True
      status `shouldBe` 200

    it "passes through valid UTF-8 in query string" $ do
      (status, passed) <- runMiddleware "/" "?q=%C3%A9"
      passed `shouldBe` True
      status `shouldBe` 200

--------------------------------------------------------------------------------

-- | Run the middleware with a crafted request and return the response
-- status code and whether the inner app was called.
runMiddleware :: ByteString -> ByteString -> IO (Int, Bool)
runMiddleware path query = do
  passedRef <- newIORef False
  let req =
        Wai.defaultRequest
          { Wai.rawPathInfo = path,
            Wai.rawQueryString = query
          }
      innerApp _req respond = do
        writeIORef passedRef True
        respond $ Wai.responseLBS HTTP.status200 [] ""
  statusRef <- newIORef 0
  _ <- validateEncodingMiddleware innerApp req $ \resp -> do
    writeIORef statusRef (HTTP.statusCode $ Wai.responseStatus resp)
    pure ResponseReceived
  status <- readIORef statusRef
  passed <- readIORef passedRef
  pure (status, passed)
