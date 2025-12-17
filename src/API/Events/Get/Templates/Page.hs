module API.Events.Get.Templates.Page
  ( header,
  )
where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Controls (renderViewControls)
import Data.Text (Text)
import Data.Time (MonthOfYear, UTCTime, Year)
import Design.Tokens qualified as Tokens
import Domain.Types.PageView (PageView)
import Effects.Database.Tables.EventTags qualified as EventTags
import Lucid qualified
import Lucid.Responsive (cls)

--------------------------------------------------------------------------------

-- | Main events template header for all views
header :: UTCTime -> PageView -> Maybe Text -> (Year, MonthOfYear) -> [EventTags.EventTagWithCount] -> Lucid.Html ()
header currentTime pageView maybeTagFilter currentMonth eventTagsWithCounts = do
  -- Events Header
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8, "text-center", Tokens.fullWidth]] $ do
    Lucid.h1_ [Lucid.class_ Tokens.heading2xl] "COMMUNITY EVENTS"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.textGray600, Tokens.mb6]] "Connect with the KPBJ community through live shows, fundraisers, and gatherings"

  -- View Controls & Filters
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    renderViewControls currentTime pageView currentMonth maybeTagFilter eventTagsWithCounts
