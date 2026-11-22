{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module API.Dashboard.Analytics.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Types
import API.Links (dashboardAnalyticsLinks, rootLink, staticAssetLink)
import API.Types (DashboardAnalyticsRoutes (..))
import Component.DashboardFrame (mainContentLoad, mainContentSwap)
import Control.Monad (forM_, when)
import Data.Aeson.Text qualified as Aeson
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine
import Servant (toUrlPiece)
import Servant.Links qualified as Links
import Text.Printf (printf)

--------------------------------------------------------------------------------

-- | Analytics dashboard page template.
template ::
  -- | The current range
  AnalyticsRange ->
  -- | The live listener count from Icecast
  Maybe Int64 ->
  -- | The analytics data for the current range
  AnalyticsData ->
  Lucid.Html ()
template range mCurrentListeners analytics = do
  -- Alpine component wrapper
  -- Chart.js is loaded dynamically in init() to work with HTMX navigation
  Lucid.div_
    [ xData_ "analyticsDashboard()"
    ]
    $ do
      -- Stats bar
      Lucid.div_ [class_ $ base [Tokens.bgMain, "px-4", "py-3", Tokens.mb4, "flex", "items-center", "justify-between"]] $ do
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2, Tokens.textSm]] $ do
          -- Live listener count
          Lucid.span_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
            Lucid.span_
              [ Lucid.class_ "w-2 h-2 rounded-full animate-pulse",
                Lucid.style_ "background: var(--theme-success);"
              ]
              mempty
            Lucid.span_ [class_ $ base [Tokens.fgMuted]] "now:"
            Lucid.span_ [class_ $ base [Tokens.fontBold], Lucid.style_ "color: var(--theme-success);"] $
              Lucid.toHtml (maybe "\x2014" (show @Int64) mCurrentListeners)
          -- Error message
          Lucid.template_ [xIf_ "error"] $
            Lucid.span_ [Lucid.style_ "color: var(--theme-error);", class_ $ base [Tokens.textXs]] $
              Lucid.span_ [xText_ "error"] mempty

        -- Time range buttons + refresh
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
          Lucid.div_ [class_ $ base ["flex", Tokens.textXs]] $
            forM_ [minBound .. maxBound] (rangeButton range)
          Lucid.a_
            ( rangeHref range
                : mainContentLoad (rangeUri range)
                  <> [class_ $ base ["px-2", "py-1", Tokens.textXs, "border", Tokens.borderMuted, Tokens.fgMuted]]
            )
            "\x21bb"

      -- Charts grid (side by side)
      Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-2", Tokens.gap4, Tokens.mb4]] $ do
        -- Listeners chart panel
        Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
          Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
            Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted]] "LISTENERS"
            Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.textXs]] $ do
              Lucid.span_ [] $ do
                Lucid.span_ [class_ $ base [Tokens.fgMuted]] "peak: "
                Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (show analytics.listeners.peak))
              Lucid.span_ [] $ do
                Lucid.span_ [class_ $ base [Tokens.fgMuted]] "avg: "
                Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (show analytics.listeners.avg))
          Lucid.div_ [Lucid.class_ "h-36"] $
            chartCanvas "listeners-chart" analytics.listeners.series

        -- Archive plays chart panel
        Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
          Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
            Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted]] "ARCHIVE PLAYS"
            Lucid.div_ [class_ $ base [Tokens.textXs]] $ do
              Lucid.span_ [class_ $ base [Tokens.fgMuted]] "total: "
              Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (show analytics.archivePlays.total))
          Lucid.div_ [Lucid.class_ "h-36"] $
            chartCanvas "plays-chart" analytics.archivePlays.series

      -- Top episodes list
      Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
        Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] "TOP EPISODES"
        forM_ analytics.topEpisodes $ \ep ->
          Lucid.div_
            [ class_ $ base ["flex", "justify-between", Tokens.textSm],
              Lucid.style_ "margin-bottom: 4px;"
            ]
            $ do
              Lucid.span_ [] $ do
                Lucid.span_ [class_ $ base [Tokens.fgMuted]] (Lucid.toHtml (show ep.rank <> "."))
                Lucid.a_ [Lucid.href_ ep.url, class_ $ base [Tokens.linkText]] (Lucid.toHtml (" " <> ep.title))
              Lucid.span_
                [ class_ $ base [Tokens.fontBold],
                  Lucid.style_ "color: var(--theme-info);"
                ]
                (Lucid.toHtml (show ep.plays))
        case analytics.topEpisodes of
          [] -> Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No episode plays in this period."
          _ -> mempty

      -- Listening by weekday and hour, and newsletter growth
      Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-2", Tokens.gap4, Tokens.mt4]] $ do
        heatmapPanel analytics.listenerHeatmap
        newsletterPanel analytics.newsletter

      -- Top shows, and Google Analytics rollups: referrers, countries, cities
      Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-4", Tokens.gap4, Tokens.mt4]] $ do
        topShowsPanel analytics.topShows
        topDimensionPanel "TOP REFERRERS" analytics.topReferrers
        topDimensionPanel "TOP COUNTRIES" analytics.topCountries
        topDimensionPanel "TOP CITIES" analytics.topCities

  -- Alpine component + Chart.js initialization script
  let chartJsUrl = rootLink $ staticAssetLink "chart.min.js"
  Lucid.script_ [] (analyticsScript chartJsUrl)

-- | Render a chart canvas with its series as JSON in @data-chart@.
chartCanvas ::
  -- | The id and the Alpine ref of the canvas
  Text ->
  ChartSeries ->
  Lucid.Html ()
chartCanvas canvasId chartSeries =
  Lucid.canvas_
    [ Lucid.id_ canvasId,
      xRef_ canvasId,
      Lucid.data_ "chart" (Lazy.toStrict (Aeson.encodeToLazyText chartSeries))
    ]
    mempty

-- | Render the average live listeners for each weekday and hour as a grid.
-- A darker cell has more listeners. A cell with no snapshots has only a
-- border.
heatmapPanel :: Heatmap -> Lucid.Html ()
heatmapPanel heatmap =
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] "LISTENERS BY HOUR (PT)"
    Lucid.div_
      [ class_ $ base ["grid", Tokens.textXs, Tokens.fgMuted],
        Lucid.style_ "grid-template-columns: 2.5rem repeat(24, minmax(0, 1fr)); gap: 2px;"
      ]
      $ do
        Lucid.div_ [] mempty
        forM_ [0 .. 23 :: Int] $ \hour ->
          Lucid.div_ [] $ if hour `mod` 6 == 0 then Lucid.toHtml (show hour) else mempty
        forM_ (zip weekdayLabels heatmap.cells) $ \(day, row) -> do
          Lucid.div_ [] (Lucid.toHtml day)
          forM_ (zip [0 :: Int ..] row) (uncurry (heatmapCell day))
    case values of
      [] -> Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-2"]] "No data in this period."
      _ -> mempty
  where
    values = catMaybes (concat heatmap.cells)
    maxValue = if null values then 0 else maximum values
    heatmapCell day hour = \case
      Nothing ->
        Lucid.div_ [Lucid.style_ "height: 14px; border: 1px solid var(--theme-border-muted);"] mempty
      Just value ->
        Lucid.div_
          [ Lucid.style_ [i|height: 14px; background: var(--theme-info); opacity: #{levelOpacity (heatmapLevel value maxValue)};|],
            Lucid.title_ (Text.pack (printf "%s %02d:00 \183 %.1f" day hour value))
          ]
          mempty

-- | The weekday labels of the heatmap rows, from Monday to Sunday.
weekdayLabels :: [String]
weekdayLabels = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

-- | The opacity of a heatmap cell for a shade from 'heatmapLevel'.
levelOpacity :: Int -> Text
levelOpacity = \case
  0 -> "0.08"
  1 -> "0.25"
  2 -> "0.45"
  3 -> "0.7"
  _ -> "1"

-- | Render the newsletter growth chart with the total and the new signups.
newsletterPanel :: NewsletterData -> Lucid.Html ()
newsletterPanel nl =
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
    Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
      Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted]] "NEWSLETTER"
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.textXs]] $ do
        Lucid.span_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fgMuted]] "total: "
          Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (show nl.total))
        Lucid.span_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fgMuted]] "new: "
          Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (show nl.new))
    Lucid.div_ [Lucid.class_ "h-36"] $
      chartCanvas "newsletter-chart" nl.series
    when (nl.total == 0) $
      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No data in this period."

-- | Render the top shows by archive plays.
topShowsPanel :: [TopShowEntry] -> Lucid.Html ()
topShowsPanel rows =
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] "TOP SHOWS"
    forM_ rows $ \row ->
      Lucid.div_
        [ class_ $ base ["flex", "justify-between", Tokens.textSm],
          Lucid.style_ "margin-bottom: 4px;"
        ]
        $ do
          Lucid.span_ [class_ $ base ["truncate", "pr-2"]] $ do
            Lucid.span_ [class_ $ base [Tokens.fgMuted]] (Lucid.toHtml (show row.rank <> "."))
            Lucid.a_ [Lucid.href_ row.url, class_ $ base [Tokens.linkText]] (Lucid.toHtml (" " <> row.title))
          Lucid.span_
            [ class_ $ base [Tokens.fontBold],
              Lucid.style_ "color: var(--theme-info);"
            ]
            (Lucid.toHtml (show row.plays))
    case rows of
      [] -> Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No episode plays in this period."
      _ -> mempty

-- | Render a top-N table panel for one Google Analytics dimension.
topDimensionPanel :: Text -> [TopDimensionEntry] -> Lucid.Html ()
topDimensionPanel heading rows =
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] (Lucid.toHtml heading)
    forM_ rows $ \row ->
      Lucid.div_
        [ class_ $ base ["flex", "justify-between", Tokens.textSm],
          Lucid.style_ "margin-bottom: 4px;"
        ]
        $ do
          Lucid.span_ [class_ $ base ["truncate", "pr-2"]] $ do
            Lucid.span_ [class_ $ base [Tokens.fgMuted]] (Lucid.toHtml (show row.rank <> "."))
            Lucid.span_ [] (Lucid.toHtml (" " <> row.label))
          Lucid.span_
            [ class_ $ base [Tokens.fontBold],
              Lucid.style_ "color: var(--theme-info);"
            ]
            (Lucid.toHtml (show row.sessions))
    case rows of
      [] -> Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No data in this period."
      _ -> mempty

-- | Render a time range button. The button for the current range is active.
rangeButton ::
  -- | The current range
  AnalyticsRange ->
  -- | The range of this button
  AnalyticsRange ->
  Lucid.Html ()
rangeButton current value =
  Lucid.a_
    (rangeHref value : swap (rangeUri value) <> [class_ $ base ["px-2", "py-1", "border", stateClasses]])
    (Lucid.toHtml (toUrlPiece value))
  where
    swap
      | value == current = mainContentLoad
      | otherwise = mainContentSwap
    stateClasses
      | value == current = "bg-[var(--theme-accent)] text-[var(--theme-accent-fg)] border-[var(--theme-border)] font-bold"
      | otherwise = "border-[var(--theme-border-muted)] text-[var(--theme-fg-muted)]"

-- | The URI of the page for a range.
--
-- A button for another range swaps the main content with 'mainContentSwap',
-- which pushes the URL, so the range is in the browser history. The button
-- for the current range and the refresh link use 'mainContentLoad', which
-- does not push the URL, because the URL is the same.
rangeUri :: AnalyticsRange -> Links.URI
rangeUri value = Links.linkURI $ dashboardAnalyticsLinks.get (Just value)

-- | The @href@ of the link to the page for a range.
rangeHref :: AnalyticsRange -> Lucid.Attributes
rangeHref value = Lucid.href_ [i|/#{rangeUri value}|]

-- | The Alpine.js + Chart.js initialization script.
analyticsScript :: Text -> Text
analyticsScript chartJsUrl =
  [i|
function analyticsDashboard() {
  // Store Chart.js instances outside Alpine's reactive scope to avoid
  // infinite recursion from Alpine proxying Chart internals.
  let listenersChart = null;
  let playsChart = null;
  let newsletterChart = null;

  return {
    error: '',

    async init() {
      try {
        await this.ensureChartJs();
        await this.$nextTick();
        // An HTMX swap can remove this component while Chart.js loads.
        if (!this.$el.isConnected) return;
        this.createCharts();
      } catch (e) {
        this.error = 'Failed to initialize charts';
        console.error('Analytics init error:', e);
      }
    },

    destroy() {
      listenersChart?.destroy();
      playsChart?.destroy();
      newsletterChart?.destroy();
      listenersChart = null;
      playsChart = null;
      newsletterChart = null;
    },

    // Each swap defines this function again, so the load promise is on
    // window. A component that starts while the script loads waits for the
    // same load.
    ensureChartJs() {
      if (typeof Chart !== 'undefined') return Promise.resolve();
      window.kpbjChartJs ??= new Promise((resolve, reject) => {
        const s = document.createElement('script');
        s.src = '#{chartJsUrl}';
        s.onload = resolve;
        s.onerror = (e) => {
          window.kpbjChartJs = null;
          reject(e);
        };
        document.head.appendChild(s);
      });
      return window.kpbjChartJs;
    },

    createCharts() {
      const lCanvas = this.$refs['listeners-chart'];
      const pCanvas = this.$refs['plays-chart'];
      const nCanvas = this.$refs['newsletter-chart'];
      if (!lCanvas || !pCanvas || !nCanvas) {
        throw new Error('Chart canvas elements not found in DOM');
      }
      const listeners = JSON.parse(lCanvas.dataset.chart);
      const plays = JSON.parse(pCanvas.dataset.chart);
      const newsletter = JSON.parse(nCanvas.dataset.chart);

      const style = getComputedStyle(document.documentElement);
      const info = style.getPropertyValue('--theme-info').trim();
      const fgMuted = style.getPropertyValue('--theme-fg-muted').trim();
      const borderMuted = style.getPropertyValue('--theme-border-muted').trim();
      const bg = style.getPropertyValue('--theme-bg').trim();
      const fg = style.getPropertyValue('--theme-fg').trim();
      const border = style.getPropertyValue('--theme-border').trim();

      const commonOptions = {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: bg,
            titleColor: fg,
            bodyColor: fg,
            borderColor: border,
            borderWidth: 1,
            titleFont: { family: 'ui-monospace, monospace' },
            bodyFont: { family: 'ui-monospace, monospace' },
          }
        },
        scales: {
          x: {
            type: 'category',
            ticks: {
              color: fgMuted,
              font: { family: 'ui-monospace, monospace', size: 10 },
              maxTicksLimit: 12,
              maxRotation: 0,
            },
            grid: { display: false },
          },
          y: {
            beginAtZero: true,
            ticks: { color: fgMuted, font: { family: 'ui-monospace, monospace', size: 10 } },
            grid: { color: borderMuted + '26' },
          }
        }
      };

      listenersChart = new Chart(lCanvas, {
        type: 'line',
        data: {
          labels: listeners.labels,
          datasets: [{
            data: listeners.values,
            borderColor: info,
            backgroundColor: info + '1a',
            fill: true,
            borderWidth: 1.5,
            pointRadius: 0,
            tension: 0.1
          }]
        },
        options: commonOptions,
      });

      playsChart = new Chart(pCanvas, {
        type: 'bar',
        data: {
          labels: plays.labels,
          datasets: [{
            data: plays.values,
            backgroundColor: info + 'b3'
          }]
        },
        options: commonOptions,
      });

      newsletterChart = new Chart(nCanvas, {
        type: 'line',
        data: {
          labels: newsletter.labels,
          datasets: [{
            data: newsletter.values,
            borderColor: info,
            borderWidth: 1.5,
            pointRadius: 0,
            stepped: true
          }]
        },
        options: { ...commonOptions, scales: { ...commonOptions.scales, y: { ...commonOptions.scales.y, beginAtZero: false } } },
      });
    }
  };
}
|]
