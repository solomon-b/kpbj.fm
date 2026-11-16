module GoogleCalendarSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import GoogleCalendar (CalendarError (..), displayError, parseListPage)
import Network.HTTP.Client (HttpException (..))
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseListPage" $ do
    it "reads an empty calendar, which has no items field" $
      parseListPage (object []) `shouldBe` Right ([], Nothing)

    it "reads the items and the token of the next page" $ do
      let item = object ["id" .= ("a" :: Text)]
      parseListPage (object ["items" .= [item], "nextPageToken" .= ("t" :: Text)])
        `shouldBe` Right ([item], Just "t")

    it "reads the last page, which has no token" $
      parseListPage (object ["items" .= ([] :: [Aeson.Value]), "nextSyncToken" .= ("s" :: Text)])
        `shouldBe` Right ([], Nothing)

    it "rejects a response that is not an object" $
      parseListPage (Aeson.String "x") `shouldSatisfy` isLeft

  -- A setup mistake shows up first as a refused list request. The reply from
  -- Google gives the reason, so the log line must carry it.
  describe "displayError" $ do
    it "gives the response body of a refused list request" $
      displayError (ListHttpStatus 403 "Calendar API has not been used in project 1 before")
        `shouldSatisfy` Text.isInfixOf "Calendar API has not been used"

    it "gives the cause of an HTTP exception" $
      displayError (ListHttpError (InvalidUrlException "http://x" "no such host"))
        `shouldSatisfy` Text.isInfixOf "no such host"

    it "gives the cause of an HTTP exception on a write" $
      displayError (WriteHttpError "Insert event" (InvalidUrlException "http://x" "no such host"))
        `shouldSatisfy` (\text -> Text.isInfixOf "Insert event" text && Text.isInfixOf "no such host" text)
