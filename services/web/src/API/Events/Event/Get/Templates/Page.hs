module API.Events.Event.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.Card.Event (renderEventCardDetail)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main event template
template :: StorageBackend -> Events.Model -> Lucid.Html () -> Lucid.Html ()
template backend event renderedDescription = do
  -- Event card with full details
  renderEventCardDetail backend event renderedDescription
