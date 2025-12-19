module API.Events.Get.Templates.Page
  ( header,
  )
where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Controls (renderViewControls)
import Component.PageHeader (pageHeader)
import Data.Text (Text)
import Data.Time (MonthOfYear, UTCTime, Year)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PageView (PageView)
import Effects.Database.Tables.EventTags qualified as EventTags
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main events template header for all views
header :: UTCTime -> PageView -> Maybe Text -> (Year, MonthOfYear) -> [EventTags.EventTagWithCount] -> Lucid.Html ()
header currentTime pageView maybeTagFilter currentMonth eventTagsWithCounts = do
  -- Events Header
  pageHeader "COMMUNITY EVENTS"

  -- View Controls & Filters
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    renderViewControls currentTime pageView currentMonth maybeTagFilter eventTagsWithCounts
