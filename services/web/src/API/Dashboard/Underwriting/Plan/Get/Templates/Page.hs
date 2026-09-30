{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The break plan view.
--
-- Each break shows its start time and its tracks in play order.
module API.Dashboard.Underwriting.Plan.Get.Templates.Page
  ( PlanView (..),
    template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritingLinks)
import API.Types (DashboardUnderwritingRoutes (..))
import Component.SourceTypeBadge (sourceTypeBadge)
import Control.Monad (forM_)
import Data.Function (on)
import Data.List (groupBy)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Timezone (formatDateLong, utcToPacific)
import Effects.Database.Tables.BreakPlans (PlannedTrack (..))
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | What the page shows for the chosen day.
data PlanView
  = -- | Today, which has a rebuild button.
    TodayPlan [PlannedTrack]
  | -- | A past day, read only.
    PastPlan [PlannedTrack]
  | -- | A future day. Plans are built on the day, so this is a forecast.
    ForecastPlan [PlannedTrack]
  deriving stock (Show, Eq)

-- | The plan page.
template :: Day -> PlanView -> Lucid.Html ()
template day view =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $ do
    Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", Tokens.gap4, Tokens.p4]] $ do
      renderDayPicker day
      case view of
        TodayPlan _ -> renderRebuildButton
        _ -> mempty
    case view of
      ForecastPlan tracks -> do
        message "Forecast. The plan is built on the day, so new orders and schedule changes will move it."
        renderBreaks tracks
      TodayPlan tracks -> renderBreaks tracks
      PastPlan tracks -> renderBreaks tracks

renderDayPicker :: Day -> Lucid.Html ()
renderDayPicker day =
  Lucid.form_
    [ hxGet_ [i|/#{planUri}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      class_ $ base ["flex", Tokens.gap4, "items-center"]
    ]
    $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold]] (Lucid.toHtml (formatDateLong day))
      Lucid.input_
        [ Lucid.type_ "date",
          Lucid.name_ "day",
          Lucid.value_ (Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)),
          class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted]
        ]
      button [Lucid.type_ "submit"] "SHOW"
  where
    planUri = Links.linkURI (dashboardUnderwritingLinks.plan Nothing)

renderRebuildButton :: Lucid.Html ()
renderRebuildButton =
  button
    [ hxPost_ [i|/#{rebuildUri}|],
      hxConfirm_ "Rebuild the plan for the rest of today? Past breaks keep their plan."
    ]
    "REBUILD TODAY'S PLAN"
  where
    rebuildUri = Links.linkURI dashboardUnderwritingLinks.planRebuild

-- | One block per break, in time order.
renderBreaks :: [PlannedTrack] -> Lucid.Html ()
renderBreaks [] = message "No breaks are planned for this day."
renderBreaks tracks =
  forM_ (groupBy ((==) `on` (.plBoundary)) tracks) $ \case
    [] -> mempty
    breakTracks@(first : _) ->
      Lucid.div_ [class_ $ base [Tokens.p4, "border-t-2", Tokens.borderMuted]] $ do
        Lucid.p_ [class_ $ base [Tokens.fontBold, Tokens.textSm, Tokens.mb2]] $
          Lucid.toHtml (breakStart first.plBoundary)
        Lucid.ol_ [class_ $ base ["space-y-1"]] $
          forM_ breakTracks $ \t ->
            Lucid.li_ [class_ $ base ["flex", Tokens.gap4, "items-center", Tokens.textSm]] $ do
              sourceTypeBadge t.plSourceType
              Lucid.span_ (Lucid.toHtml t.plTitle)

-- | The Pacific time a break starts, two minutes before its boundary.
breakStart :: UTCTime -> Text
breakStart boundary =
  Text.pack (formatTime defaultTimeLocale "%I:%M %p" (utcToPacific (addUTCTime (-120) boundary)))

message :: Text -> Lucid.Html ()
message = Lucid.p_ [class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]] . Lucid.toHtml

button :: [Lucid.Attributes] -> Text -> Lucid.Html ()
button attrs label =
  Lucid.button_
    ( attrs
        <> [class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]]
    )
    (Lucid.toHtml label)
