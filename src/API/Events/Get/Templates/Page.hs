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
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main events page template
template :: StorageBackend -> [Events.Model] -> Lucid.Html ()
template backend events = do
  pageHeader "EVENTS"

  Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
    Lucid.div_ [class_ $ base ["max-w-lg", "mx-auto", "space-y-8"]] $ do
      if null events
        then Lucid.div_ [Lucid.class_ Tokens.cardBase] $ do
          Lucid.div_ [Lucid.class_ "text-center"] $ do
            Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "No Events Scheduled"
            Lucid.p_ [Lucid.class_ Tokens.fgMuted] "Check back soon for upcoming community events!"
        else traverse_ (renderEventCardSummary backend) events
