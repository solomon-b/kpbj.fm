{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Get.Templates.ShowHeader
  ( renderShowHeader,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Types
import Component.Tags qualified as Tags
import Control.Monad (forM_, unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..))
import Design (base, class_, desktop)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render show header with info
renderShowHeader :: StorageBackend -> Shows.Model -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowTags.Model] -> Lucid.Html ()
renderShowHeader backend showModel hosts schedules tags = do
  Lucid.section_ [class_ $ do { base [Tokens.bgWhite, Tokens.mb8, Tokens.fullWidth]; desktop [Tokens.p8] }] $ do
    Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap8]; desktop ["grid-cols-4"] }] $ do
      -- Show Logo
      Lucid.div_ [class_ $ desktop ["col-span-1"]] $ do
        Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-[4/3]", "bg-gray-300", "border", "border-gray-300", "flex", "items-center", "justify-center", Tokens.textLg]] $ do
          case showModel.logoUrl of
            Just logoPath -> do
              let logoAlt = showModel.title <> " logo"
              Lucid.img_ [Lucid.src_ (buildMediaUrl backend logoPath), Lucid.alt_ logoAlt, Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

      -- Show Info
      Lucid.div_ [class_ $ desktop ["col-span-3"]] $ do
        Lucid.div_ [Lucid.class_ Tokens.mb4] $ do
          Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb2]] $ Lucid.toHtml (Text.toUpper showModel.title)

          -- Show metadata: vertical on mobile, horizontal on desktop
          Lucid.div_ [class_ $ do { base ["flex", "flex-col", "gap-1", Tokens.textGray600, Tokens.mb4]; desktop ["flex-row", "items-center", Tokens.gap6, Tokens.textLg] }] $ do
            -- Host (bolder and bigger on mobile)
            Lucid.div_ [class_ $ do { base [Tokens.textLg, Tokens.fontBold, Tokens.textGray800]; desktop ["font-normal", Tokens.textGray600] }] $
              case hosts of
                [] -> ""
                (ShowHost.ShowHostWithUser {displayName = dn} : otherHosts) -> do
                  Lucid.toHtml dn
                  unless (null otherHosts) $ do
                    ", "
                    let otherNames = map (\(ShowHost.ShowHostWithUser {displayName = n}) -> Lucid.toHtml n) otherHosts
                    mconcat $ map (", " <>) otherNames
            -- Schedule
            Lucid.div_ $
              case schedules of
                [] -> ""
                (ShowSchedule.ScheduleTemplate {stDayOfWeek = mDow, stStartTime = st, stEndTime = et} : _) ->
                  case mDow of
                    Nothing -> "One-time show"
                    Just dow -> do
                      let dayName :: Text
                          dayName = case dow of
                            Sunday -> "Sunday"
                            Monday -> "Monday"
                            Tuesday -> "Tuesday"
                            Wednesday -> "Wednesday"
                            Thursday -> "Thursday"
                            Friday -> "Friday"
                            Saturday -> "Saturday"
                      Lucid.toHtml $ dayName <> "s " <> formatTimeOfDay st <> " - " <> formatTimeOfDay et

        -- Show Description (only if present)
        forM_ showModel.description $ \description ->
          Lucid.div_ [class_ $ do { base [Tokens.mb6, "pb-4", "border-b", "border-gray-800"]; desktop ["border-b-0", "pb-0"] }] $ do
            Lucid.p_ [class_ $ base ["leading-relaxed"]] $ Lucid.toHtml description

        -- Tags
        unless (null tags) $
          Tags.renderTags (map showTagToLink tags)
  where
    showTagToLink :: ShowTags.Model -> Tags.TagLink
    showTagToLink tag =
      Tags.TagLink
        { tagName = ShowTags.stName tag,
          tagUrl = Links.linkURI $ showsLinks.list Nothing (Just (Filter (Just (ShowTags.stId tag)))) Nothing Nothing Nothing
        }
