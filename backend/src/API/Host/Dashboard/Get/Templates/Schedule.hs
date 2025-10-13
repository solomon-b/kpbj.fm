{-# LANGUAGE OverloadedRecordDot #-}

module API.Host.Dashboard.Get.Templates.Schedule
  ( renderScheduleSection,
  )
where

import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

-- | Render schedule sidebar section
renderScheduleSection :: Shows.Model -> Lucid.Html ()
renderScheduleSection userShow = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-4"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "YOUR SCHEDULE"
    Lucid.div_ [Lucid.class_ "text-center text-sm mb-4"] $ do
      Lucid.div_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml userShow.title
      Lucid.div_ [Lucid.class_ "mb-1"] "Schedule: TBD" -- TODO: Add schedule info
      Lucid.div_ [Lucid.class_ "text-xs text-gray-300"] "Set by station management"

    Lucid.div_ [Lucid.class_ "bg-gray-700 p-3 mb-4 text-xs"] $ do
      Lucid.div_ [Lucid.class_ "font-bold mb-2"] "NEXT SHOW:"
      Lucid.div_ "TBD - Schedule to be determined"
      Lucid.div_ [Lucid.class_ "text-gray-300"] "Contact management for scheduling"

    Lucid.div_ [Lucid.class_ "space-y-2"] $ do
      Lucid.button_ [Lucid.class_ "w-full bg-green-600 text-white py-2 text-xs font-bold hover:bg-green-700"] $ do
        "VIEW FULL SCHEDULE"
