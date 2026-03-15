{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module API.Dashboard.Analytics.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Links (dashboardAnalyticsLinks, staticAssetLink, rootLink)
import API.Types (DashboardAnalyticsRoutes (..))
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

analyticsDataUrl :: Links.URI
analyticsDataUrl = Links.linkURI $ dashboardAnalyticsLinks.dataGet Nothing

-- | Analytics dashboard page template.
template :: Maybe Int64 -> Lucid.Html ()
template mCurrentListeners = do
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
          Lucid.div_ [class_ $ base ["flex", Tokens.textXs]] $ do
            rangeButton "24h"
            rangeButton "7d"
            rangeButton "30d"
            rangeButton "90d"
          Lucid.button_
            [ class_ $ base ["px-2", "py-1", Tokens.textXs, "border", Tokens.borderMuted, Tokens.fgMuted],
              xOn_ "click" "refresh()"
            ]
            "\x21bb"

      -- Charts grid (side by side)
      Lucid.div_ [class_ $ base ["grid", "grid-cols-2", Tokens.gap4, Tokens.mb4]] $ do
        -- Listeners chart panel
        Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
          Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
            Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted]] "LISTENERS"
            Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.textXs]] $ do
              Lucid.span_ [] $ do
                Lucid.span_ [class_ $ base [Tokens.fgMuted]] "peak: "
                Lucid.span_ [class_ $ base [Tokens.fontBold], xText_ "listeners.peak || '\x2014'"] mempty
              Lucid.span_ [] $ do
                Lucid.span_ [class_ $ base [Tokens.fgMuted]] "avg: "
                Lucid.span_ [class_ $ base [Tokens.fontBold], xText_ "listeners.avg || '\x2014'"] mempty
          Lucid.div_ [Lucid.class_ "h-36"] $
            Lucid.canvas_ [Lucid.id_ "listeners-chart"] mempty

        -- Archive plays chart panel
        Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
          Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.mb2]] $ do
            Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted]] "ARCHIVE PLAYS"
            Lucid.div_ [class_ $ base [Tokens.textXs]] $ do
              Lucid.span_ [class_ $ base [Tokens.fgMuted]] "total: "
              Lucid.span_ [class_ $ base [Tokens.fontBold], xText_ "archivePlays.total || '\x2014'"] mempty
          Lucid.div_ [Lucid.class_ "h-36"] $
            Lucid.canvas_ [Lucid.id_ "plays-chart"] mempty

      -- Top episodes list
      Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.p3]] $ do
        Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] "TOP EPISODES"
        Lucid.template_ [xFor_ "ep in topEpisodes", xKey_ "ep.rank"]
          $ Lucid.div_
            [ class_ $ base ["flex", "justify-between", Tokens.textSm],
              Lucid.style_ "margin-bottom: 4px;"
            ]
          $ do
            Lucid.span_ [] $ do
              Lucid.span_ [class_ $ base [Tokens.fgMuted], xText_ "ep.rank + '.'"] mempty
              Lucid.span_ [xText_ "' ' + ep.title"] mempty
            Lucid.span_
              [ class_ $ base [Tokens.fontBold],
                Lucid.style_ "color: var(--theme-info);",
                xText_ "ep.plays"
              ]
              mempty
        Lucid.template_ [xIf_ "topEpisodes.length === 0 && !loading"] $
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No episode plays in this period."

  -- Alpine component + Chart.js initialization script
  let chartJsUrl = rootLink $ staticAssetLink "chart.min.js"
  Lucid.script_ [] (analyticsScript analyticsDataUrl chartJsUrl)

-- | Render a time range button with active highlighting.
rangeButton :: Text -> Lucid.Html ()
rangeButton value =
  Lucid.button_
    [ class_ $ base ["px-2", "py-1", "border"],
      xBindClass_ [i|range === '#{value}' ? 'bg-[var(--theme-accent)] text-[var(--theme-accent-fg)] border-[var(--theme-border)] font-bold' : 'border-[var(--theme-border-muted)] text-[var(--theme-fg-muted)]'|],
      xOn_ "click" [i|setRange('#{value}')|]
    ]
    (Lucid.toHtml value)

-- | The Alpine.js + Chart.js initialization script.
analyticsScript :: Links.URI -> Text -> Text
analyticsScript dataUrl chartJsUrl =
  [i|
function analyticsDashboard() {
  // Store Chart.js instances outside Alpine's reactive scope to avoid
  // infinite recursion from Alpine proxying Chart internals.
  let listenersChart = null;
  let playsChart = null;

  return {
    range: '7d',
    listeners: { peak: null, avg: null },
    archivePlays: { total: null },
    topEpisodes: [],
    loading: false,
    error: '',

    async init() {
      try {
        await this.ensureChartJs();
        await this.$nextTick();
        this.createCharts();
        this.fetchData();
      } catch (e) {
        this.error = 'Failed to initialize charts';
        console.error('Analytics init error:', e);
      }
    },

    ensureChartJs() {
      if (typeof Chart !== 'undefined') return Promise.resolve();
      return new Promise((resolve, reject) => {
        const s = document.createElement('script');
        s.src = '#{chartJsUrl}';
        s.onload = resolve;
        s.onerror = reject;
        document.head.appendChild(s);
      });
    },

    createCharts() {
      const lCanvas = document.getElementById('listeners-chart');
      const pCanvas = document.getElementById('plays-chart');
      if (!lCanvas || !pCanvas) {
        throw new Error('Chart canvas elements not found in DOM');
      }

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
          labels: [],
          datasets: [{
            data: [],
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
          labels: [],
          datasets: [{
            data: [],
            backgroundColor: info + 'b3'
          }]
        },
        options: commonOptions,
      });
    },

    async fetchData() {
      this.loading = true;
      this.error = '';
      try {
        const res = await fetch('/#{dataUrl}?range=' + this.range);
        if (!res.ok) throw new Error('Failed to load data');
        const data = await res.json();

        this.listeners = { peak: data.listeners.peak, avg: data.listeners.avg };
        this.archivePlays = { total: data.archivePlays.total };
        this.topEpisodes = data.topEpisodes;

        if (listenersChart) {
          listenersChart.data.labels = data.listeners.labels;
          listenersChart.data.datasets[0].data = data.listeners.data;
          listenersChart.update();
        }

        if (playsChart) {
          playsChart.data.labels = data.archivePlays.labels;
          playsChart.data.datasets[0].data = data.archivePlays.data;
          playsChart.update();
        }
      } catch (e) {
        this.error = e.message || 'Failed to load data';
      } finally {
        this.loading = false;
      }
    },

    setRange(r) {
      this.range = r;
      this.fetchData();
    },

    refresh() {
      this.fetchData();
    }
  };
}
|]
