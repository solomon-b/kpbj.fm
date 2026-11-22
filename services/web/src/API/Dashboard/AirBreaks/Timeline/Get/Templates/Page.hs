{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The day timeline view.
--
-- Every item of a Pacific day in play order, grouped by segment. Problems show
-- in a bar above the table, each with a link to its segment.
module API.Dashboard.AirBreaks.Timeline.Get.Templates.Page
  ( DayKind (..),
    TimelineView (..),
    template,
    dayNav,
  )
where

--------------------------------------------------------------------------------

import API.Links
  ( dashboardAnnouncementsLinks,
    dashboardEphemeralUploadsLinks,
    dashboardEpisodesLinks,
    dashboardPromosLinks,
    dashboardPsasLinks,
    dashboardStationIdsLinks,
    dashboardTimelineLink,
    dashboardUnderwritingLinks,
  )
import API.Types
  ( DashboardAnnouncementsRoutes (..),
    DashboardEphemeralUploadsRoutes (..),
    DashboardEpisodesRoutes (..),
    DashboardPromosRoutes (..),
    DashboardPsasRoutes (..),
    DashboardStationIdsRoutes (..),
    DashboardUnderwritingRoutes (..),
  )
import Component.Button ((&))
import Component.Button qualified as Button
import Component.SourceTypeBadge (sourceTypeBadge)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, addDays, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.DayTimeline (Flag (..), Problem (..), SegmentView (..))
import Domain.Timeline (BreakTrack (..), Content (..), EphemeralCandidate (..), EpisodeCandidate (..), Item (..), Segment (..))
import Domain.Types.Timezone (formatDateLong, utcToPacific)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Lucid.Alpine
import Lucid.Base qualified as LucidBase
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Where the chosen day sits relative to today.
data DayKind = PastDay | Today | FutureDay
  deriving stock (Show, Eq)

-- | Everything the page shows.
data TimelineView = TimelineView
  { tvDay :: Day,
    tvKind :: DayKind,
    -- | The load time. Only today uses it.
    tvNow :: UTCTime,
    tvSegments :: [SegmentView],
    tvProblems :: [Problem]
  }
  deriving stock (Show, Eq)

-- | The timeline page.
template :: TimelineView -> Lucid.Html ()
template view =
  Lucid.div_ [class_ $ base ["flex", "flex-col", Tokens.gap4, Tokens.mb8]] $ do
    renderProblems view.tvProblems
    renderTable view

--------------------------------------------------------------------------------
-- Day navigation

-- | The day navigation, for the top bar.
--
-- An HTMX response brings the top bar out of band, so the navigation shows
-- the new day after a swap.
dayNav :: TimelineView -> Lucid.Html ()
dayNav view =
  Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.fgPrimary]] $
    do
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
        dayLink (addDays (-1) day) [i|< #{shortDay (addDays (-1) day)}|]
        Lucid.span_ [class_ $ base [Tokens.fontBold, "whitespace-nowrap"]] (Lucid.toHtml (formatDateLong day))
        dayLink (addDays 1 day) [i|#{shortDay (addDays 1 day)} >|]
      Lucid.form_
        [ hxGet_ [i|/#{timelineUri Nothing}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["flex", Tokens.gap2, "items-center"]
        ]
        $ do
          Lucid.input_
            [ Lucid.type_ "date",
              Lucid.name_ "day",
              Lucid.value_ (dayParam day),
              LucidBase.makeAttributes "aria-label" "Day",
              class_ $ base ["px-2", "py-1", Tokens.textSm, Tokens.bgMain, Tokens.fgPrimary, "border", Tokens.borderMuted]
            ]
          Button.render (Button.defaults & Button.small & Button.submit) "SHOW"
          when (view.tvKind == Today) $
            -- The link scrolls by script and adds no entry to the browser
            -- history. A back step from a @#now@ entry loads the page again.
            Button.render
              ( Button.defaults
                  & Button.small
                  & Button.pageLink "#now"
                  & Button.with [Lucid.onclick_ "document.getElementById('now').scrollIntoView(); return false;"]
              )
              "NOW"
  where
    day = view.tvDay

dayLink :: Day -> Text -> Lucid.Html ()
dayLink day label =
  Button.render (Button.defaults & Button.small & Button.dashboardLink [i|/#{uri}|]) (Lucid.toHtml label)
  where
    uri = timelineUri (Just (dayParam day))

timelineUri :: Maybe Text -> Links.URI
timelineUri = Links.linkURI . dashboardTimelineLink

dayParam :: Day -> Text
dayParam = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- | A day as @FRI NOV 13@.
shortDay :: Day -> Text
shortDay = Text.toUpper . Text.pack . formatTime defaultTimeLocale "%a %b %d"

--------------------------------------------------------------------------------
-- Problems

renderProblems :: [Problem] -> Lucid.Html ()
renderProblems [] = mempty
renderProblems ps =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "border-2", Tokens.errorBorder, Tokens.p4, "flex", "flex-wrap", "items-center", "gap-x-6", "gap-y-2", Tokens.textSm]] $ do
    Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.errorText]] $
      Lucid.toHtml (problemCount (length ps))
    forM_ ps $ \p ->
      Lucid.a_ [Lucid.href_ ("#" <> segmentAnchor p.problemSegmentStart), class_ $ base ["underline"]] $
        Lucid.toHtml (problemText p)
  where
    problemCount :: Int -> Text
    problemCount 1 = "1 PROBLEM"
    problemCount n = [i|#{n} PROBLEMS|]

-- | The problem bar line of a problem.
problemText :: Problem -> Text
problemText p = case p.problemFlag of
  NoPlayableEpisode window ->
    Text.unwords [clockTime window.swStart, showTitle window, "has no playable episode"]
  LongFiller seconds ->
    Text.unwords
      [ clockTime p.problemSegmentStart,
        maybe "unscheduled hour" (.swShowTitle) p.problemSlot,
        "runs",
        Text.pack (show (minutes seconds)),
        "min of filler"
      ]
  DroppedSlot window mAiring ->
    Text.unwords [clockTime window.swStart, showTitle window, "does not air"]
      <> maybe "" (\airing -> ": it overlaps " <> showTitle airing) mAiring

-- | The show title of a slot window, in capitals.
showTitle :: Episodes.SlotWindow -> Text
showTitle window = Text.toUpper window.swShowTitle

--------------------------------------------------------------------------------
-- The table

renderTable :: TimelineView -> Lucid.Html ()
renderTable view
  | null view.tvSegments =
      Lucid.p_ [class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]] "Nothing is scheduled for this day."
  | otherwise =
      Lucid.div_
        [ class_ $ base [Tokens.bgMain, "border-2", Tokens.borderDefault, "overflow-x-auto"],
          xData_ "{}",
          xInit_ "$nextTick(() => document.getElementById('now')?.scrollIntoView({block: 'center'}))"
        ]
        $ Lucid.table_ [class_ $ base ["w-full", "border-collapse", Tokens.textSm]]
        $ do
          Lucid.thead_ $
            Lucid.tr_ $
              forM_ ["START", "LENGTH", "TYPE", "TITLE", "CUE"] $ \heading ->
                Lucid.th_ [Lucid.scope_ "col", class_ $ base ["text-left", Tokens.textXs, Tokens.fgMuted, Tokens.fontBold, Tokens.px3, Tokens.py2, "border-b-2", Tokens.borderDefault]] heading
          Lucid.tbody_ $
            forM_ view.tvSegments (renderSegment view)

renderSegment :: TimelineView -> SegmentView -> Lucid.Html ()
renderSegment view sv = do
  Lucid.tr_ [Lucid.id_ (segmentAnchor segment.segStart)] $
    Lucid.td_ [Lucid.colspan_ "5", class_ $ base [Tokens.px3, Tokens.py2, Tokens.fontBold, Tokens.bgAlt, "border-t-2", Tokens.borderDefault]] $ do
      Lucid.toHtml (clockTime segment.segStart <> " – " <> clockTime segment.segEnd <> " · " <> maybe "UNSCHEDULED" showTitle sv.svSlot)
      forM_ sv.svFlags $ \flag ->
        Lucid.span_ [class_ $ base [Tokens.errorText]] $ Lucid.toHtml (" · " <> flagText flag)
  forM_ (fillerRuns segment.segItems) $ \case
    Single item -> renderItem view sv item
    FillerRun first rest -> do
      renderItem view sv first
      when (any (isCurrentItem view) rest) nowMarker
      Lucid.tr_ [class_ $ base [Tokens.errorBg]] $ do
        Lucid.td_ [class_ $ base cellClasses] (Lucid.toHtml (clockTimeSeconds (itemPlannedStart (NonEmpty.head rest))))
        Lucid.td_ [Lucid.colspan_ "4", class_ $ base (Tokens.errorText : cellClasses)] $
          Lucid.toHtml ([i|… #{length rest} more filler items, #{duration (sum (fmap itemSeconds rest))} in all|] :: Text)
  where
    segment = sv.svSegment

-- | The flag text on a segment row.
flagText :: Flag -> Text
flagText = \case
  NoPlayableEpisode _ -> "NO PLAYABLE EPISODE"
  LongFiller seconds -> [i|#{minutes seconds} MIN FILLER|]
  DroppedSlot window _ -> showTitle window <> " DOES NOT AIR"

renderItem :: TimelineView -> SegmentView -> Item -> Lucid.Html ()
renderItem view sv item = do
  when isCurrent nowMarker
  Lucid.tr_ [class_ $ base rowClasses] $ do
    Lucid.td_ [class_ $ base cellClasses] $
      Lucid.toHtml (clockTimeSeconds (itemPlannedStart item))
    Lucid.td_ [class_ $ base cellClasses] $ Lucid.toHtml (duration (itemSeconds item))
    Lucid.td_ [class_ $ base cellClasses] $ sourceTypeBadge sourceType
    Lucid.td_ [class_ $ base ["w-full", "whitespace-normal", Tokens.px3, "py-1", "border-t", Tokens.borderMuted]] $ do
      case itemLink sv (itemContent item) of
        Just uri -> Lucid.a_ [Lucid.href_ [i|/#{uri}|], class_ $ base ["underline"]] (Lucid.toHtml title)
        Nothing -> Lucid.toHtml title
      when isCurrent $
        Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fontBold, Tokens.warningText, "ml-2"]] "ON AIR"
    Lucid.td_ [class_ $ base cellClasses] $ Lucid.toHtml (cueText item)
  where
    (sourceType, title) = describe (itemContent item)
    isCurrent = isCurrentItem view item
    isProblem = case itemContent item of
      FillerContent -> any isLongFiller sv.svFlags
      EphemeralContent _ -> any isNoEpisode sv.svFlags
      _ -> False
    rowClasses =
      [Tokens.warningBg | isCurrent]
        <> [Tokens.errorBg | isProblem, not isCurrent]

-- | The row that marks the current time.
nowMarker :: Lucid.Html ()
nowMarker =
  Lucid.tr_ [Lucid.id_ "now"] $
    Lucid.td_ [Lucid.colspan_ "5", class_ $ base ["p-0", "border-t-2", Tokens.warningBorder]] mempty

-- | True when the item is on air now.
isCurrentItem :: TimelineView -> Item -> Bool
isCurrentItem view item =
  view.tvKind == Today && itemStart item <= view.tvNow && view.tvNow < itemEnd item

-- | The instant the item stops.
itemEnd :: Item -> UTCTime
itemEnd item = addUTCTime (fromIntegral (itemSeconds item)) (itemStart item)

isLongFiller :: Flag -> Bool
isLongFiller = \case
  LongFiller _ -> True
  _ -> False

isNoEpisode :: Flag -> Bool
isNoEpisode = \case
  NoPlayableEpisode _ -> True
  _ -> False

cellClasses :: [Text]
cellClasses = [Tokens.px3, "py-1", "border-t", Tokens.borderMuted, "whitespace-nowrap"]

-- | The source type and the title of an item, as @/api/playout/now@ names
-- them.
describe :: Content -> (Text, Text)
describe = \case
  EpisodeContent episode -> ("episode", ecShowTitle episode)
  EphemeralContent upload -> ("ephemeral", ecdTitle upload)
  BreakContent track -> (btSourceType track, btTitle track)
  FillerContent -> ("filler", "KPBJ filler")

-- | The edit page of an item, when it has one.
itemLink :: SegmentView -> Content -> Maybe Links.URI
itemLink sv = \case
  EpisodeContent _ -> do
    window <- sv.svSlot
    episode <- window.swEpisode
    pure (Links.linkURI (dashboardEpisodesLinks.detail window.swShowSlug episode.seEpisodeNumber))
  EphemeralContent upload ->
    Just (Links.linkURI (dashboardEphemeralUploadsLinks.editGet (ecdId upload)))
  BreakContent track -> case btSourceType track of
    "psa" -> Links.linkURI . dashboardPsasLinks.editGet <$> breakItemId
    "announcement" -> Links.linkURI . dashboardAnnouncementsLinks.editGet <$> breakItemId
    "underwriting" -> Links.linkURI . dashboardUnderwritingLinks.editGet <$> breakItemId
    "show_promo" -> Just (Links.linkURI (dashboardPromosLinks.list Nothing))
    "station_id" -> Just (Links.linkURI (dashboardStationIdsLinks.list Nothing))
    _ -> Nothing
    where
      breakItemId = BreakItems.Id <$> btBreakItemId track
  FillerContent -> Nothing

-- | The cue column. At build time every cue in is 0, so only a cut item has
-- something to show. A filler item always has a cue out, so it shows none.
cueText :: Item -> Text
cueText item = case (itemContent item, itemCueOut item) of
  (FillerContent, _) -> ""
  (_, Just cueOut) -> [i|cut at #{duration cueOut}|]
  (_, Nothing) -> ""

--------------------------------------------------------------------------------
-- Filler runs

-- | A row group of the table.
data Row
  = Single Item
  | -- | Two or more filler items in a row. The first shows in full, the rest
    -- as one summary row.
    FillerRun Item (NonEmpty Item)

-- | Group consecutive filler items, so an empty hour takes two rows.
fillerRuns :: [Item] -> [Row]
fillerRuns = \case
  [] -> []
  item : rest
    | isFiller item ->
        let (more, after) = span isFiller rest
         in maybe (Single item) (FillerRun item) (NonEmpty.nonEmpty more) : fillerRuns after
    | otherwise -> Single item : fillerRuns rest
  where
    isFiller i' = case itemContent i' of
      FillerContent -> True
      _ -> False

--------------------------------------------------------------------------------
-- Formatting

-- | The anchor id of a segment row.
segmentAnchor :: UTCTime -> Text
segmentAnchor start = [i|segment-#{posixSeconds}|]
  where
    posixSeconds = floor (utcTimeToPOSIXSeconds start) :: Int64

-- | A Pacific time as @1:00 PM@.
clockTime :: UTCTime -> Text
clockTime = Text.strip . Text.pack . formatTime defaultTimeLocale "%l:%M %p" . utcToPacific

-- | A Pacific time with seconds, as @1:00:00 PM@.
clockTimeSeconds :: UTCTime -> Text
clockTimeSeconds = Text.strip . Text.pack . formatTime defaultTimeLocale "%l:%M:%S %p" . utcToPacific

-- | A length as @m:ss@, or @h:mm:ss@ from one hour up.
duration :: Int64 -> Text
duration seconds
  | h > 0 = [i|#{h}:#{pad m}:#{pad s}|]
  | otherwise = [i|#{m}:#{pad s}|]
  where
    (h, rest) = seconds `divMod` 3600
    (m, s) = rest `divMod` 60
    pad n = if n < 10 then "0" <> Text.pack (show n) else Text.pack (show n)

-- | Whole minutes, rounded down.
minutes :: Int64 -> Int64
minutes seconds = seconds `div` 60
