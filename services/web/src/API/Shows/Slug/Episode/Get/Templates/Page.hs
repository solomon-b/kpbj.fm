{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Get.Templates.Page
  ( template,
    errorTemplate,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Types
import Component.Card.Episode (renderEpisodeCard)
import Component.Tags qualified as Tags
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

showsListUrl :: Links.URI
showsListUrl = Links.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing Nothing

-- | Convert episode tags to tag links.
-- Links to shows list for now (no episode tag filtering route yet).
episodeTagToLink :: EpisodeTags.Model -> Tags.TagLink
episodeTagToLink tag =
  Tags.TagLink
    { tagName = EpisodeTags.etName tag,
      tagUrl = showsListUrl -- TODO: Link to proper episode tag filtering when implemented
    }

--------------------------------------------------------------------------------

template :: StorageBackend -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> [EpisodeTags.Model] -> Lucid.Html ()
template backend showModel episode tracks tags = do
  Lucid.div_ [class_ $ base [Tokens.fullWidth]] $ do
    -- Episode card (reuses the same component from show page)
    renderEpisodeCard backend showModel episode

    -- Tags section
    unless (null tags) $ do
      Lucid.div_ [class_ $ base [Tokens.bgMain, "mt-6"]] $ do
        Tags.renderTags (map episodeTagToLink tags)

    -- Track listing section
    unless (null tracks) $ do
      Lucid.div_ [class_ $ base [Tokens.bgMain, "mt-6"]] $ do
        Lucid.h2_
          [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderDefault, Tokens.pb2]]
          "Track Listing"

        Lucid.div_ [Lucid.class_ "space-y-1"] $ do
          mapM_ renderTrackRow tracks

--------------------------------------------------------------------------------

renderTrackRow :: EpisodeTrack.Model -> Lucid.Html ()
renderTrackRow track = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", Tokens.p3, Tokens.hoverBg, "border-b", Tokens.borderMuted]] $ do
    -- Track number
    Lucid.div_ [class_ $ base ["w-8", "flex-shrink-0", Tokens.fgMuted, "font-mono", Tokens.textSm]] $
      Lucid.toHtml (show track.trackNumber <> ".")

    -- Track info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.div_ [Lucid.class_ "font-medium"] $ do
        "\"" <> Lucid.toHtml track.title <> "\""

      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $ do
        Lucid.toHtml track.artist

--------------------------------------------------------------------------------

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [class_ $ base [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
      Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4, Tokens.errorText]] "Error Loading Episode"
      Lucid.p_ [class_ $ base [Tokens.errorText, Tokens.mb6]] $ Lucid.toHtml errorMsg
      Lucid.a_
        [ Lucid.href_ "/shows",
          hxGet_ "/shows",
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["inline-block", Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:opacity-80"]
        ]
        "Back to Shows"

notFoundTemplate :: Slug -> Episodes.EpisodeNumber -> Lucid.Html ()
notFoundTemplate showSlug episodeNumber = do
  let showUrl = showGetUrl showSlug
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, "p-8", "text-center"]] $ do
      Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "Episode Not Found"
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] $
        "We couldn't find Episode " <> Lucid.toHtml (display episodeNumber) <> " for show \"" <> Lucid.toHtml (display showSlug) <> "\"."
      Lucid.a_
        [ Lucid.href_ [i|/#{showUrl}|],
          hxGet_ [i|/#{showUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["inline-block", Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:opacity-80"]
        ]
        "Back to Show"
