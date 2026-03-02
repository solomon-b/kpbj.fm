module Middleware.ValidateEncodingSpec (spec) where

--------------------------------------------------------------------------------

import Control.Exception (evaluate)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text.Encoding qualified as Text
import Middleware.ValidateEncoding (validateEncodingMiddleware)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Middleware.ValidateEncoding" $ do
  describe "validateEncodingMiddleware" $ do
    it "passes through a normal request" $ do
      status <- runMiddleware $ \_req respond ->
        respond $ Wai.responseLBS HTTP.status200 [] ""
      status `shouldBe` 200

    it "catches UnicodeException from inner app and returns 400" $ do
      status <- runMiddleware $ \_req _respond -> do
        -- Force a strict decode of invalid UTF-8; throws UnicodeException
        _ <- evaluate (Text.decodeUtf8 "\xAD")
        pure ResponseReceived
      status `shouldBe` 400

    it "does not catch non-Unicode exceptions" $ do
      runMiddleware (\_ _ -> error "not a unicode exception")
        `shouldThrow` anyErrorCall

--------------------------------------------------------------------------------

-- | Run the middleware with a given inner app and return the response
-- status code.
runMiddleware :: Wai.Application -> IO Int
runMiddleware innerApp = do
  statusRef <- newIORef 0
  _ <- validateEncodingMiddleware innerApp Wai.defaultRequest $ \resp -> do
    writeIORef statusRef (HTTP.statusCode $ Wai.responseStatus resp)
    pure ResponseReceived
  readIORef statusRef
