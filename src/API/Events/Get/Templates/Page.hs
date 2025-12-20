module API.Events.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.Card.Event (renderEventCardSummary)
import Component.PageHeader (pageHeader)
import Data.Foldable (traverse_)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main events page template
template :: [Events.Model] -> Lucid.Html ()
template events = do
  pageHeader "COMMUNITY EVENTS"
  Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
    Lucid.div_ [class_ $ base ["max-w-lg", "mx-auto", "space-y-8"]] $ do
      if null events
        then Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
          Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Events Scheduled"
          Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for upcoming community events!"
        else traverse_ renderEventCardSummary events
