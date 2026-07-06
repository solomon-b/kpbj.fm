{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Dashboard editor for an event's photo gallery.
--
-- A self-contained Alpine.js multi-image manager, modeled on the form-builder's
-- @renderImagesField@ but with two deliberate differences:
--
-- 1. __No cropping.__ Selected photos are added as-is, preserving their original
--    aspect ratio (event photos are a mix of portrait and landscape).
-- 2. __Visible caption__ per photo, in addition to alt text.
--
-- It is embedded into the event edit form via the form-builder's @plain@ escape
-- hatch, so its hidden inputs submit with the existing multipart POST. The wire
-- contract mirrors @renderImagesField@:
--
--   * @event_gallery_files@   — multipart, the newly added files
--   * @event_gallery_data@    — JSON @[{id, sort_order, alt_text, caption}]@
--   * @event_gallery_deleted@ — JSON @[id, ...]@ of removed existing photos
module Component.Gallery.EventGalleryEditor
  ( GalleryImageView (..),
    renderEventGalleryEditor,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)

--------------------------------------------------------------------------------

-- | A view of one existing gallery photo, used to seed the Alpine state.
data GalleryImageView = GalleryImageView
  { givId :: Int64,
    givUrl :: Text,
    givCaption :: Text,
    givAltText :: Text
  }

instance ToJSON GalleryImageView where
  toJSON img =
    object
      [ "id" .= givId img,
        "url" .= givUrl img,
        "caption" .= givCaption img,
        "alt_text" .= givAltText img
      ]

--------------------------------------------------------------------------------

-- | Render the gallery editor section. @maxSizeMB@ bounds each individual photo.
renderEventGalleryEditor :: Int -> [GalleryImageView] -> Lucid.Html ()
renderEventGalleryEditor maxSizeMB existingImages = do
  let existingJson :: Text
      existingJson = Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode existingImages

  Lucid.section_
    [ Lucid.class_ "fb-section",
      xData_
        [i|{
  images: #{existingJson}.map((img, idx) => ({...img, isNew: false, file: null, previewUrl: img.url, _key: 'existing-' + (img.id ?? idx)})),
  _nextKey: 0,
  deletedIds: [],
  error: '',
  dragIdx: null,
  dropIdx: null,
  isDragging: false,

  addFiles(fileList) {
    const files = Array.from(fileList || []);
    for (const file of files) {
      if (!file.type.startsWith('image/')) continue;
      if (file.size > #{maxSizeMB} * 1024 * 1024) {
        this.error = 'One or more files exceed the #{maxSizeMB}MB limit and were skipped.';
        continue;
      }
      this.images.push({
        id: null, url: '', caption: '', alt_text: '', isNew: true,
        file: file, previewUrl: URL.createObjectURL(file),
        _key: 'new-' + (this._nextKey++),
      });
    }
    this.syncFiles();
  },

  handleFileChange(event) {
    this.error = '';
    this.addFiles(event.target.files);
    event.target.value = '';
  },

  handleFileDrop(event) {
    this.isDragging = false;
    this.error = '';
    this.addFiles(event.dataTransfer?.files);
  },

  removeImage(idx) {
    const img = this.images[idx];
    if (!img.isNew && img.id) { this.deletedIds.push(img.id); }
    if (img.previewUrl && img.isNew) { URL.revokeObjectURL(img.previewUrl); }
    this.images.splice(idx, 1);
    this.syncFiles();
  },

  handleDragStart(e, idx) { this.dragIdx = idx; e.dataTransfer.effectAllowed = 'move'; },
  handleDragOver(e, idx) { e.preventDefault(); this.dropIdx = idx; },
  handleDragDrop(e, idx) {
    e.preventDefault();
    if (this.dragIdx === null || this.dragIdx === idx) return;
    const item = this.images.splice(this.dragIdx, 1)[0];
    this.images.splice(idx, 0, item);
    this.dragIdx = null;
    this.dropIdx = null;
    this.syncFiles();
  },
  handleDragEnd(e) { this.dragIdx = null; this.dropIdx = null; },

  syncFiles() {
    const input = this.$refs.filesInput;
    if (!input) return;
    const dt = new DataTransfer();
    this.images.filter(img => img.isNew && img.file).forEach(img => {
      dt.items.add(img.file);
    });
    input.files = dt.files;
  },

  serializeData() {
    return JSON.stringify(this.images.map((img, idx) => ({
      id: img.id, sort_order: idx, alt_text: img.alt_text || '', caption: img.caption || '',
    })));
  },

  serializeDeleted() {
    return JSON.stringify(this.deletedIds);
  },
}|]
    ]
    $ do
      Lucid.h2_ [Lucid.class_ "fb-section-title"] "EVENT PHOTO GALLERY"
      Lucid.p_ [Lucid.class_ "fb-hint"] "Optional. Post-event photos, shown as a gallery on the public event page. Drag to reorder \x2014 photos keep their original shape."

      -- Error display
      Lucid.div_
        [ xShow_ "error",
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        $ Lucid.span_ [xText_ "error"] ""

      -- Image grid
      Lucid.div_
        [ Lucid.class_ "fb-images-grid",
          Lucid.style_ "--fb-images-thumb-size: 180px;"
        ]
        $ do
          Lucid.template_ [xFor_ "(img, idx) in images", xKey_ "img._key"]
            $ Lucid.div_
              [ makeAttributes "draggable" "true",
                xOn_ "dragstart" "handleDragStart($event, idx)",
                xOn_ "dragover.prevent" "handleDragOver($event, idx)",
                xOn_ "drop.prevent" "handleDragDrop($event, idx)",
                xOn_ "dragend" "handleDragEnd($event)",
                xBindClass_ "{ 'fb-images-card--drag-over': dropIdx === idx }",
                Lucid.class_ "fb-images-card"
              ]
            $ do
              -- Image preview (native aspect ratio)
              Lucid.img_
                [ xBindSrc_ "img.previewUrl || img.url",
                  Lucid.class_ "eg-card-img",
                  Lucid.alt_ "Photo preview"
                ]

              -- Remove button
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "removeImage(idx)",
                  Lucid.class_ "fb-images-remove"
                ]
                "\215"

              -- Caption input (visible on the public page)
              Lucid.input_
                [ Lucid.type_ "text",
                  xModel_ "img.caption",
                  Lucid.placeholder_ "Caption (optional)",
                  Lucid.class_ "fb-images-alt-input"
                ]

              -- Alt text input (accessibility)
              Lucid.input_
                [ Lucid.type_ "text",
                  xModel_ "img.alt_text",
                  Lucid.placeholder_ "Alt text (accessibility)",
                  Lucid.class_ "fb-images-alt-input"
                ]

          -- Add zone
          Lucid.div_
            [ xOnClick_ "$refs.addInput.click()",
              xOnDragover_ "isDragging = true",
              xOnDragleave_ "isDragging = false",
              xOnDrop_ "handleFileDrop($event)",
              Lucid.class_ "fb-images-add-zone"
            ]
            $ Lucid.span_ [Lucid.class_ "fb-images-add-label"] "+ Add Photos"

      -- Hidden file input for adding photos (supports selecting several at once)
      Lucid.input_
        [ Lucid.type_ "file",
          Lucid.id_ "event-gallery-add-input",
          Lucid.accept_ "image/jpeg,image/jpg,image/png,image/webp,image/gif",
          makeAttributes "multiple" "multiple",
          xRef_ "addInput",
          xOnChange_ "handleFileChange($event)",
          Lucid.style_ "display: none;"
        ]

      -- Hidden file input that holds all new photo files for form submission
      Lucid.input_
        [ Lucid.type_ "file",
          Lucid.name_ "event_gallery_files",
          makeAttributes "multiple" "multiple",
          xRef_ "filesInput",
          Lucid.style_ "display: none;"
        ]

      -- Hidden metadata (image data JSON)
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ "event_gallery_data",
          xBindValue_ "serializeData()"
        ]

      -- Hidden deleted IDs
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ "event_gallery_deleted",
          xBindValue_ "serializeDeleted()"
        ]
