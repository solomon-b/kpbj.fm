module API.Dashboard.Analytics.Get.Templates.PageSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Templates.Page (template)
import API.Dashboard.Analytics.Get.Types
import Data.Text.Lazy qualified as Lazy
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "API.Dashboard.Analytics.Get.Templates.Page" $ do
  let html = Lucid.renderText (template Range30d (Just 5) fixture)

  it "links each range to the page with hx-get into #main-content" $ do
    html `shouldSatisfy` Lazy.isInfixOf "href=\"/dashboard/analytics?range=24h\""
    html `shouldSatisfy` Lazy.isInfixOf "hx-get=\"/dashboard/analytics?range=90d\""
    html `shouldSatisfy` Lazy.isInfixOf "href=\"/dashboard/analytics?range=all\""
    html `shouldSatisfy` Lazy.isInfixOf "hx-target=\"#main-content\""
    html `shouldSatisfy` Lazy.isInfixOf "hx-push-url=\"true\""

  it "marks only the current range as active" $ do
    anchorsTo "/dashboard/analytics?range=30d" html `shouldSatisfy` any (Lazy.isInfixOf activeClass)
    anchorsTo "/dashboard/analytics?range=7d" html `shouldNotSatisfy` any (Lazy.isInfixOf activeClass)

  it "renders the stats as text" $ do
    html `shouldSatisfy` Lazy.isInfixOf ">7<"
    html `shouldSatisfy` Lazy.isInfixOf ">3.8<"
    html `shouldSatisfy` Lazy.isInfixOf ">12<"
    html `shouldSatisfy` Lazy.isInfixOf ">5<"

  it "renders top episodes as links" $
    html `shouldSatisfy` Lazy.isInfixOf "href=\"/shows/jazz-hour/episodes/5\""

  it "escapes a GA label" $ do
    html `shouldSatisfy` Lazy.isInfixOf "&lt;script&gt;alert(1)&lt;/script&gt;"
    html `shouldNotSatisfy` Lazy.isInfixOf "<script>alert(1)"

  it "puts the chart series in data-chart" $
    html
      `shouldSatisfy` Lazy.isInfixOf
        "data-chart=\"{&quot;labels&quot;:[&quot;2026-11-20T00:00:00Z&quot;,&quot;2026-11-20T01:00:00Z&quot;],&quot;values&quot;:[3,4.5]}\""

  it "renders empty states for a range with no data" $ do
    let empty = Lucid.renderText (template Range7d Nothing emptyData)
    empty `shouldSatisfy` Lazy.isInfixOf "No episode plays in this period."
    empty `shouldSatisfy` Lazy.isInfixOf "No data in this period."
    empty `shouldSatisfy` Lazy.isInfixOf "{&quot;labels&quot;:[],&quot;values&quot;:[]}"

--------------------------------------------------------------------------------

-- | A class that only the active range button has.
activeClass :: Lazy.Text
activeClass = "bg-[var(--theme-accent)]"

-- | The opening tags of every @<a>@ element with the given @href@.
anchorsTo :: Lazy.Text -> Lazy.Text -> [Lazy.Text]
anchorsTo href =
  filter (Lazy.isInfixOf ("href=\"" <> href <> "\""))
    . map (Lazy.takeWhile (/= '>'))
    . drop 1
    . Lazy.splitOn "<a "

fixture :: AnalyticsData
fixture =
  AnalyticsData
    { listeners =
        ListenerData
          { series = ChartSeries ["2026-11-20T00:00:00Z", "2026-11-20T01:00:00Z"] [3, 4.5],
            peak = 7,
            avg = 3.8
          },
      archivePlays = ArchivePlayData {series = ChartSeries ["2026-11-20"] [12], total = 12},
      topEpisodes =
        [TopEpisode {rank = 1, title = "Jazz Hour #5", showTitle = "Jazz Hour", plays = 9, url = "/shows/jazz-hour/episodes/5"}],
      topReferrers = [TopDimensionEntry {rank = 1, label = "<script>alert(1)</script>", sessions = 40}],
      topCountries = [],
      topCities = []
    }

emptyData :: AnalyticsData
emptyData =
  AnalyticsData
    { listeners = ListenerData {series = ChartSeries [] [], peak = 0, avg = 0},
      archivePlays = ArchivePlayData {series = ChartSeries [] [], total = 0},
      topEpisodes = [],
      topReferrers = [],
      topCountries = [],
      topCities = []
    }
