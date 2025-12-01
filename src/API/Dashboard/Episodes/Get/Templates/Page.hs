{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Get.Templates.Page
  ( template,
  )
where

import {-# SOURCE #-} API (episodesNewGetLink)
import API.Dashboard.Get.Templates.Episode (renderEpisodeTableRow)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTable)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OrphanInstances.DayOfWeek (dayOfWeekToText)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Servant.Links qualified as Links

-- | Episodes dashboard template
template ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [Episodes.Model] ->
  [ShowSchedule.ScheduleTemplate] ->
  Maybe ShowSchedule.UpcomingShowDate ->
  Lucid.Html ()
template userMeta selectedShow episodes schedules nextShow = do
  renderHeader userMeta selectedShow schedules nextShow (length episodes)
  renderEpisodesSection userMeta selectedShow episodes

-- | Header with stats
renderHeader ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [ShowSchedule.ScheduleTemplate] ->
  Maybe ShowSchedule.UpcomingShowDate ->
  Int ->
  Lucid.Html ()
renderHeader userMeta selectedShow schedules nextShow episodeCount =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 rounded-lg"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EPISODES"
        let isAdmin = UserMetadata.isAdmin userMeta.mUserRole
        if not isAdmin
          then Lucid.div_ [Lucid.class_ "text-gray-300 text-sm mb-2"] $ do
            Lucid.strong_ "Schedule: "
            renderScheduleInfo schedules
            case nextShow of
              Just upcoming -> do
                " • "
                Lucid.strong_ "Next Show: "
                Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" (ShowSchedule.usdShowDate upcoming)
              Nothing -> mempty
          else mempty
        Lucid.div_ [Lucid.class_ "text-sm mt-2"] $ do
          Lucid.strong_ [Lucid.class_ "text-gray-400"] "Total Episodes: "
          Lucid.span_ [Lucid.class_ "text-white"] $ Lucid.toHtml $ show episodeCount
      -- Action button
      case selectedShow of
        Just showModel -> do
          let uploadUrl = Links.linkURI $ episodesNewGetLink showModel.slug
          Lucid.a_
            [ Lucid.href_ [i|/#{uploadUrl}|],
              hxGet_ [i|/#{uploadUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
            ]
            "PREPARE SHOW"
        Nothing -> mempty

-- | Format schedule info
renderScheduleInfo :: [ShowSchedule.ScheduleTemplate] -> Lucid.Html ()
renderScheduleInfo [] = "Not scheduled"
renderScheduleInfo (firstSchedule : rest) =
  let allSchedules = firstSchedule : rest
      dayNames = mapMaybe (fmap dayOfWeekToText . (.dayOfWeek)) allSchedules
      dayText = if null dayNames then "One-time show" else Text.intercalate ", " dayNames
      timeRange = formatTimeOfDay firstSchedule.startTime <> " - " <> formatTimeOfDay firstSchedule.endTime
   in Lucid.toHtml $ dayText <> " • " <> timeRange

-- | Episodes table section
renderEpisodesSection :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> Lucid.Html ()
renderEpisodesSection userMeta selectedShow episodes =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    case episodes of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No episodes uploaded yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Use 'PREPARE SHOW' to upload your first episode."
      _ ->
        renderTable
          TableConfig
            { headers =
                [ ColumnHeader "#" AlignLeft,
                  ColumnHeader "TITLE" AlignLeft,
                  ColumnHeader "SCHEDULED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          $ mapM_ (maybe (const mempty) (renderEpisodeTableRow userMeta) selectedShow) episodes
