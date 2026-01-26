{-# LANGUAGE QuasiQuotes #-}

module API.Schedule.Get.Templates.Components
  ( renderSchedule,
  )
where

import API.Links (showsLinks)
import API.Types
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (DayOfWeek (..), TimeOfDay (..))
import Design (also, base, class_, desktop, unless, when)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (Attributes)
import Lucid.HTMX
import OrphanInstances.DayOfWeek (fromDayOfWeek)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Servant.Links qualified as Links

-- | Create URL for show page
showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

-- | Get the next day of the week (for overnight show handling)
succDay :: DayOfWeek -> DayOfWeek
succDay Sunday = Monday
succDay Monday = Tuesday
succDay Tuesday = Wednesday
succDay Wednesday = Thursday
succDay Thursday = Friday
succDay Friday = Saturday
succDay Saturday = Sunday

-- | Check if a show is currently live, handling overnight shows
--
-- For overnight shows (where end_time <= start_time), the show spans two calendar days.
-- E.g., Tuesday 23:00 - 00:00 means Tuesday 23:00 to Wednesday 00:00.
isShowLive :: Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> TimeOfDay -> TimeOfDay -> Bool
isShowLive mCurrentDay mCurrentTime showDay startTime endTime =
  case (mCurrentDay, mCurrentTime) of
    (Just currentDay, Just currentTime) ->
      let isOvernight = endTime <= startTime
       in if isOvernight
            then -- Show crosses midnight: live if (same day AND after start) OR (next day AND before end)
              (currentDay == showDay && currentTime >= startTime)
                || (currentDay == succDay showDay && currentTime < endTime)
            else -- Normal same-day show
              currentDay == showDay
                && currentTime >= startTime
                && currentTime < endTime
    _ -> False

-- | Render schedule (single day view with day navigation)
renderSchedule :: StorageBackend -> [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderSchedule backend scheduledShows currentDayOfWeek currentTimeOfDay = do
  let daysOfWeek = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
      -- Default to current day, or Monday if not viewing current week
      initialDayIndex :: Int
      initialDayIndex = maybe 0 (fromIntegral . fromDayOfWeek) currentDayOfWeek
      alpineData = [i|{ currentDay: #{initialDayIndex} }|]

  Lucid.div_ [containerStyles, xData_ alpineData] $ do
    -- Header row: "Schedule" on left, day nav on right
    Lucid.div_ [headerRowStyles] $ do
      -- Spacer to align with time column (hidden on mobile)
      Lucid.div_ [headerSpacerStyles] mempty
      -- Header content (aligns with card)
      Lucid.div_ [headerContentStyles] $ do
        Lucid.h1_ [headerTitleStyles] "Schedule"
        -- Day navigation
        Lucid.div_ [dayNavStyles] $ do
          -- Previous day button
          Lucid.button_
            [ dayNavButtonStyles,
              xOnClick_ "currentDay = currentDay > 0 ? currentDay - 1 : 6"
            ]
            "←"

          -- Current day label
          Lucid.span_ [dayLabelStyles] $ do
            mapM_ (renderDayLabel currentDayOfWeek) (zip [0 ..] daysOfWeek)

          -- Next day button
          Lucid.button_
            [ dayNavButtonStyles,
              xOnClick_ "currentDay = currentDay < 6 ? currentDay + 1 : 0"
            ]
            "→"

    -- Day content panels (one per day, shown/hidden by Alpine)
    mapM_ (renderMobileDayPanel backend scheduledShows currentDayOfWeek currentTimeOfDay) (zip [0 ..] daysOfWeek)

--------------------------------------------------------------------------------
-- Schedule Styles

containerStyles :: Attributes
containerStyles = class_ $ do
  base ["space-y-4"]

headerRowStyles :: Attributes
headerRowStyles = class_ $ do
  base ["flex", "items-center", Tokens.mb4, "gap-3"]
  desktop ["justify-center"]

headerSpacerStyles :: Attributes
headerSpacerStyles = class_ $ do
  base ["hidden"]
  desktop ["block", "w-20", "flex-shrink-0"]

headerContentStyles :: Attributes
headerContentStyles = class_ $ do
  base ["flex", "justify-between", "items-center", "flex-grow"]
  desktop ["flex-grow-0", "w-1/2"]

headerTitleStyles :: Attributes
headerTitleStyles = class_ $ do
  base [Tokens.text2xl, Tokens.fontBold]

dayNavStyles :: Attributes
dayNavStyles = class_ $ do
  base ["flex", "items-center"]

dayNavButtonStyles :: Attributes
dayNavButtonStyles = class_ $ do
  base [Tokens.textXl, "font-black", Tokens.px3, Tokens.py2]
  also ["hover:text-gray-600", "cursor-pointer"]

dayLabelStyles :: Attributes
dayLabelStyles = class_ $ do
  base [Tokens.fontBold, Tokens.textLg]

-- | Render a day label that shows/hides based on Alpine state
renderDayLabel :: Maybe DayOfWeek -> (Int, DayOfWeek) -> Lucid.Html ()
renderDayLabel currentDayOfWeek (idx, day) = do
  let isToday = Just day == currentDayOfWeek
      dayText = if isToday then "Today" else dayOfWeekName day
      showCondition = [i|currentDay === #{idx :: Int}|]
  Lucid.span_ [xShow_ showCondition] $ Lucid.toHtml dayText

-- | Get display name for day of week
dayOfWeekName :: DayOfWeek -> Text
dayOfWeekName Monday = "Monday"
dayOfWeekName Tuesday = "Tuesday"
dayOfWeekName Wednesday = "Wednesday"
dayOfWeekName Thursday = "Thursday"
dayOfWeekName Friday = "Friday"
dayOfWeekName Saturday = "Saturday"
dayOfWeekName Sunday = "Sunday"

-- | Render a day panel (shown/hidden by Alpine based on currentDay)
renderMobileDayPanel :: StorageBackend -> [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> (Int, DayOfWeek) -> Lucid.Html ()
renderMobileDayPanel backend scheduledShows currentDayOfWeek currentTimeOfDay (idx, dayOfWeek) = do
  let showsForDay = filter (\s -> ShowSchedule.sswdDayOfWeek s == dayOfWeek) scheduledShows
      sortedShows = sortBy (\a b -> compare (ShowSchedule.sswdStartTime a) (ShowSchedule.sswdStartTime b)) showsForDay
      showCondition = [i|currentDay === #{idx :: Int}|]

  Lucid.div_ [xShow_ showCondition, Lucid.class_ "space-y-3"] $ do
    if null sortedShows
      then Lucid.p_ [mobileEmptyStyles] "No shows scheduled"
      else mapM_ (renderShowCard backend currentDayOfWeek currentTimeOfDay dayOfWeek) sortedShows

mobileEmptyStyles :: Attributes
mobileEmptyStyles = class_ $ do
  base [Tokens.textGray600, "text-center", Tokens.py4, "italic"]

-- | Render a single show card with logo image
renderShowCard :: StorageBackend -> Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> ShowSchedule.ScheduledShowWithDetails -> Lucid.Html ()
renderShowCard backend currentDayOfWeek currentTimeOfDay dayOfWeek show' = do
  let showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
      startTimeText = formatTimeOfDay (ShowSchedule.sswdStartTime show')
      isLiveShow = isShowLive currentDayOfWeek currentTimeOfDay dayOfWeek (ShowSchedule.sswdStartTime show') (ShowSchedule.sswdEndTime show')
      mLogoPath = ShowSchedule.sswdLogoUrl show'
      hasImage = isJust mLogoPath

  Lucid.div_ [showRowStyles] $ do
    -- Time label on left (with LIVE indicator below if applicable)
    Lucid.div_ [timeContainerStyles] $ do
      Lucid.div_ [timeStyles] $ Lucid.toHtml startTimeText
      if isLiveShow
        then Lucid.div_ [liveBadgeStyles] "*ON AIR*"
        else mempty

    -- Show card on right
    Lucid.a_
      [ Lucid.href_ [i|/#{showUrl}|],
        hxGet_ [i|/#{showUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        showCardStyles isLiveShow hasImage
      ]
      $ do
        -- Logo image
        case mLogoPath of
          Just logoPath ->
            Lucid.img_
              [ Lucid.src_ (buildMediaUrl backend logoPath),
                Lucid.alt_ "",
                logoImageStyles
              ]
          Nothing -> mempty
        -- Show title (with text shadow when there's an image for readability)
        Lucid.div_ [showTitleStyles hasImage] $ Lucid.toHtml (ShowSchedule.sswdShowTitle show')

showRowStyles :: Attributes
showRowStyles = class_ $ do
  base ["flex", "items-start", "gap-3"]
  desktop ["justify-center"]

timeContainerStyles :: Attributes
timeContainerStyles = class_ $ do
  base ["w-20", "flex-shrink-0", "pt-3"]

timeStyles :: Attributes
timeStyles = class_ $ do
  base [Tokens.textSm, Tokens.textGray600]

showCardStyles :: Bool -> Bool -> Attributes
showCardStyles isLive hasImage = class_ $ do
  base [Tokens.p3, "block", "flex", "items-end", "flex-grow"]
  base ["relative", "overflow-hidden"]
  base ["aspect-[4/3]"]
  desktop ["flex-grow-0", "w-1/2"]
  unless hasImage $ base [Tokens.bgWhite]
  base ["border", "border-gray-300"]
  when isLive $ base [Tokens.border2, "border-gray-800"]
  unless hasImage $ also ["hover:bg-gray-50"]
  when hasImage $ also ["hover:opacity-90"]

-- | Logo image styles
logoImageStyles :: Attributes
logoImageStyles = class_ $ do
  base ["absolute", "inset-0", "w-full", "h-full", "object-cover"]

showTitleStyles :: Bool -> Attributes
showTitleStyles hasImage = class_ $ do
  base [Tokens.fontBold, "relative", "z-10"]
  desktop [Tokens.text2xl]
  when hasImage $ base [Tokens.textWhite, "drop-shadow-[0_2px_4px_rgba(0,0,0,0.8)]"]

liveBadgeStyles :: Attributes
liveBadgeStyles = class_ $ do
  base [Tokens.fontBold, Tokens.textXs, Tokens.textGray800, "animate-pulse"]
