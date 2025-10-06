{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.Page
  ( header,
  )
where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Controls (renderViewControls)
import Data.Text (Text)
import Data.Time (MonthOfYear, UTCTime, Year)
import Domain.Types.PageView (PageView)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main events template header for all views
header :: UTCTime -> PageView -> Maybe Text -> (Year, MonthOfYear) -> [Events.EventTagWithCount] -> Lucid.Html ()
header currentTime pageView maybeTagFilter currentMonth eventTagsWithCounts = do
  -- Events Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "COMMUNITY EVENTS"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Connect with the KPBJ community through live shows, fundraisers, and gatherings"

  -- View Controls & Filters
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    renderViewControls currentTime pageView currentMonth maybeTagFilter eventTagsWithCounts
