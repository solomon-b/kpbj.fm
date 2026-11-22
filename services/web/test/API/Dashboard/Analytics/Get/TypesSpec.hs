module API.Dashboard.Analytics.Get.TypesSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Types (AnalyticsRange (..), ChartSeries (..))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec
import Web.HttpApiData (parseQueryParam, toQueryParam)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "API.Dashboard.Analytics.Get.Types" $ do
  describe "AnalyticsRange" $ do
    it "round-trips through the query param form" $
      for_ [minBound .. maxBound :: AnalyticsRange] $ \r ->
        parseQueryParam (toQueryParam r) `shouldBe` Right r

    it "uses the short range names" $
      map toQueryParam [Range24h, Range7d, Range30d, Range90d] `shouldBe` ["24h", "7d", "30d", "90d"]

    it "rejects an unknown range" $
      (parseQueryParam "1y" :: Either Text AnalyticsRange) `shouldSatisfy` isLeft

  describe "ChartSeries" $
    it "encodes labels and values" $
      Aeson.encode (ChartSeries ["a", "b"] [1, 2.5]) `shouldBe` "{\"labels\":[\"a\",\"b\"],\"values\":[1,2.5]}"
