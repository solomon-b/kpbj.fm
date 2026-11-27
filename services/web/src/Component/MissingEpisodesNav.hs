{-# LANGUAGE QuasiQuotes #-}

-- | The links between the two views of the Missing Episodes section.
module Component.MissingEpisodesNav
  ( View (..),
    render,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardMissingEpisodesLink, dashboardMissingEpisodesPastLink)
import Component.Button ((&))
import Component.Button qualified as Button
import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | A view of the Missing Episodes section.
data View
  = -- | The slots of the next 7 days with no upload.
    Upcoming
  | -- | The finished slots that aired no new episode.
    Past
  deriving stock (Eq, Show, Enum, Bounded)

-- | One link for each view. The link of the current view has the primary
-- style.
render ::
  -- | The view on screen
  View ->
  Lucid.Html ()
render current =
  Lucid.nav_ [class_ $ base ["flex", Tokens.gap2, Tokens.mb4]] $
    forM_ [minBound .. maxBound] $ \view ->
      Button.render
        (Button.defaults & style view & Button.small & Button.dashboardLink (viewUrl view))
        (Lucid.toHtml (viewLabel view))
  where
    style :: View -> Button.Spec -> Button.Spec
    style view
      | view == current = Button.primary
      | otherwise = Button.secondary

viewLabel :: View -> Text
viewLabel = \case
  Upcoming -> "UPCOMING"
  Past -> "PAST"

viewUrl :: View -> Text
viewUrl = \case
  Upcoming -> [i|/#{Links.linkURI dashboardMissingEpisodesLink}|]
  Past -> [i|/#{Links.linkURI (dashboardMissingEpisodesPastLink Nothing)}|]
