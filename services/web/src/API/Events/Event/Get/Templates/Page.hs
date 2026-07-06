module API.Events.Event.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.Card.Event (renderEventCardDetail)
import Component.Gallery.EventGalleryView (renderEventGallery)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.EventImages qualified as EventImages
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main event template
template :: StorageBackend -> Events.Model -> [EventImages.Model] -> Lucid.Html () -> Lucid.Html ()
template backend event images renderedDescription = do
  -- Event card with full details
  renderEventCardDetail backend event renderedDescription

  -- Optional post-event photo gallery (renders nothing when empty)
  renderEventGallery backend images
