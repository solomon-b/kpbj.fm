module Google.ServiceAccountSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson (Value, (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Google.ServiceAccount (displayTokenError, httpCause, makeClaimsSet, parseServiceAccountKey)
import Google.ServiceAccount qualified as ServiceAccount
import Network.HTTP.Client (HttpException (..))
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "makeClaimsSet" $ do
    it "has no sub claim when the account acts as itself" $
      claim "sub" (claims Nothing) `shouldBe` Just (Nothing :: Maybe Text)

    it "names the Workspace user in the sub claim" $
      claim "sub" (claims (Just "admin@example.com")) `shouldBe` Just (Just ("admin@example.com" :: Text))

    it "carries the scope" $
      claim "scope" (claims Nothing) `shouldBe` Just (Just ("https://example.com/scope" :: Text))

    it "expires one hour after the issue time" $ do
      let issued = claim "iat" (claims Nothing) :: Maybe (Maybe Int)
          expires = claim "exp" (claims Nothing) :: Maybe (Maybe Int)
      (fmap . fmap) (+ 3600) issued `shouldBe` expires
      issued `shouldSatisfy` (/= Just Nothing)

  describe "parseServiceAccountKey" $
    it "rejects text with no private key" $
      parseServiceAccountKey "not a key" `shouldSatisfy` isLeft

  -- The request of an authorized call holds the access token.
  describe "httpCause" $
    it "gives the reason and not the URL" $
      httpCause (InvalidUrlException "http://secret" "no such host") `shouldBe` "no such host"

  describe "displayTokenError" $
    it "gives the cause of an HTTP exception" $
      displayTokenError (ServiceAccount.TokenExchangeHttpError (InvalidUrlException "http://x" "no such host"))
        `shouldSatisfy` Text.isInfixOf "no such host"

--------------------------------------------------------------------------------

-- | The claims of a fixed account at a fixed time, as JSON.
claims :: Maybe Text -> Maybe Value
claims subject =
  either (const Nothing) (Just . Aeson.toJSON) $
    makeClaimsSet "job@example.iam.gserviceaccount.com" subject "https://example.com/scope" (UTCTime (fromGregorian 2026 1 1) 0)

-- | One claim. The outer 'Nothing' means the claims are not an object.
claim :: (Aeson.FromJSON a) => Aeson.Key -> Maybe Value -> Maybe (Maybe a)
claim name value = value >>= parseMaybe (Aeson.withObject "claims" (.:? name))
