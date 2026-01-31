{-# LANGUAGE OverloadedRecordDot #-}

module API.Events.Event.Get.Templates.Page
  ( template,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import Component.Card.Event (renderEventCardDetail)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main event template
template :: StorageBackend -> Events.Model -> Lucid.Html () -> Lucid.Html ()
template backend event renderedDescription = do
  -- Event card with full details
  renderEventCardDetail backend event renderedDescription

-- | Template for when event is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb4]] "Event Not Found"
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] $ do
      "The event with slug \""
      Lucid.code_ [class_ $ base [Tokens.bgAlt, "px-2", "py-1"]] $ Lucid.toHtml (display slug)
      "\" could not be found."
