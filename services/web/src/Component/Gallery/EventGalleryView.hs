{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Public read-only display of an event's photo gallery.
--
-- Renders a responsive thumbnail grid (photos keep their original aspect ratio)
-- with a click-to-enlarge Alpine.js lightbox. Pure client-side — no server
-- round-trips. Renders nothing when the event has no photos.
module Component.Gallery.EventGalleryView
  ( renderEventGallery,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EventImages qualified as EventImages
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)

--------------------------------------------------------------------------------

-- | Minimal per-photo payload for the lightbox Alpine state.
data GalleryPhoto = GalleryPhoto
  { gpUrl :: Text,
    gpCaption :: Text,
    gpAlt :: Text
  }

instance ToJSON GalleryPhoto where
  toJSON p =
    object
      [ "url" .= gpUrl p,
        "caption" .= gpCaption p,
        "alt" .= gpAlt p
      ]

--------------------------------------------------------------------------------

-- | Render the gallery section, or nothing if there are no photos.
renderEventGallery :: StorageBackend -> [EventImages.Model] -> Lucid.Html ()
renderEventGallery _ [] = mempty
renderEventGallery backend images = do
  let photos = map toPhoto images
      photosJson :: Text
      photosJson = Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode photos

  Lucid.section_
    [ -- w-full: #main-content is a flex column with items-center on desktop, which
      -- would otherwise shrink this section to its content width and collapse the grid.
      Lucid.class_ "mt-8 w-full",
      xData_ [i|{ open: false, idx: 0, photos: #{photosJson} }|]
    ]
    $ do
      Lucid.h2_ [Lucid.class_ "text-lg font-bold mb-4 border-b-2 pb-2"] "PHOTOS"

      -- Thumbnail grid
      Lucid.div_ [Lucid.class_ "eg-gallery-grid"] $
        mapM_ renderThumb (zip [0 :: Int ..] photos)

      -- Lightbox overlay
      renderLightbox
  where
    toPhoto :: EventImages.Model -> GalleryPhoto
    toPhoto img =
      GalleryPhoto
        { gpUrl = buildMediaUrl backend (EventImages.eviImagePath img),
          gpCaption = EventImages.eviCaption img,
          gpAlt = EventImages.eviAltText img
        }

    renderThumb :: (Int, GalleryPhoto) -> Lucid.Html ()
    renderThumb (idx, photo) =
      Lucid.button_
        [ Lucid.type_ "button",
          Lucid.class_ "eg-gallery-item",
          xOnClick_ [i|idx = #{idx}; open = true|]
        ]
        $ do
          Lucid.img_
            [ Lucid.src_ (gpUrl photo),
              Lucid.alt_ (gpAlt photo),
              Lucid.class_ "eg-gallery-thumb",
              makeAttributes "loading" "lazy"
            ]
          case gpCaption photo of
            "" -> mempty
            caption -> Lucid.div_ [Lucid.class_ "eg-gallery-caption"] (Lucid.toHtml caption)

    renderLightbox :: Lucid.Html ()
    renderLightbox =
      Lucid.div_
        [ xShow_ "open",
          Lucid.style_ "display: none;",
          xOn_ "keydown.escape.window" "open = false",
          Lucid.class_ "eg-lightbox",
          xOnClick_ "open = false"
        ]
        $ do
          -- Close button
          Lucid.button_
            [ Lucid.type_ "button",
              Lucid.class_ "eg-lightbox-close",
              xOnClick_ "open = false"
            ]
            "\215"

          -- Previous
          Lucid.button_
            [ Lucid.type_ "button",
              Lucid.class_ "eg-lightbox-nav eg-lightbox-nav--prev",
              xShow_ "photos.length > 1",
              xOn_ "click.stop" "idx = (idx - 1 + photos.length) % photos.length"
            ]
            "\8249"

          -- Next
          Lucid.button_
            [ Lucid.type_ "button",
              Lucid.class_ "eg-lightbox-nav eg-lightbox-nav--next",
              xShow_ "photos.length > 1",
              xOn_ "click.stop" "idx = (idx + 1) % photos.length"
            ]
            "\8250"

          -- Enlarged image + caption (stop propagation so clicking the image doesn't close)
          Lucid.img_
            [ xBindSrc_ "photos[idx].url",
              makeAttributes "x-bind:alt" "photos[idx].alt",
              Lucid.class_ "eg-lightbox-img",
              xOn_ "click.stop" ""
            ]
          Lucid.div_
            [ Lucid.class_ "eg-lightbox-caption",
              xShow_ "photos[idx].caption",
              xText_ "photos[idx].caption"
            ]
            mempty
