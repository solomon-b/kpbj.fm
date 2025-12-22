{-# LANGUAGE QuasiQuotes #-}

module Component.ImageFilePicker
  ( Config (..),
    render,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras (xBindClass_, xBindSrc_, xData_, xOnChange_, xOnClick_, xOnDragleave_, xOnDragover_, xOnDrop_, xRef_, xShow_, xText_)

--------------------------------------------------------------------------------

-- | Configuration for the image file picker component.
data Config = Config
  { -- | Field name for the file input
    fieldName :: Text,
    -- | Display label for the field
    label :: Text,
    -- | URL to existing image (empty string if none)
    existingImageUrl :: Text,
    -- | Accepted file types
    accept :: Text,
    -- | Maximum file size in MB
    maxSizeMB :: Int,
    -- | Whether this field is required
    isRequired :: Bool,
    -- | Aspect ratio as (width, height), e.g., (1, 1) for square, (16, 9) for widescreen
    aspectRatio :: (Int, Int)
  }

--------------------------------------------------------------------------------

-- | Convert MIME types to human-readable format
-- e.g., "image/jpeg,image/png,image/webp" -> "JPG, PNG, WEBP"
humanReadableTypes :: Text -> Text
humanReadableTypes accept
  | accept == "image/*" = "All image types"
  | otherwise =
      let types = Text.splitOn "," accept
          toHuman t = case Text.strip t of
            "image/jpeg" -> "JPG"
            "image/jpg" -> "JPG"
            "image/png" -> "PNG"
            "image/webp" -> "WEBP"
            "image/gif" -> "GIF"
            "image/svg+xml" -> "SVG"
            other -> Text.toUpper $ Text.replace "image/" "" other
       in Text.intercalate ", " $ map toHuman types

--------------------------------------------------------------------------------

-- | Render an image file picker with integrated preview and cropping.
--
-- Features:
-- - Drag-and-drop support with visual feedback
-- - Click anywhere in the drop zone to select
-- - Cropper.js integration for aspect ratio cropping
-- - Human-readable file type display
render :: Config -> Lucid.Html ()
render Config {..} = do
  let labelText :: Text
      labelText = if isRequired then label <> " *" else label
      inputId = fieldName <> "-input"
      hasExisting = existingImageUrl /= ""
      (ratioW, ratioH) = aspectRatio
      aspectRatioStyle :: Text
      aspectRatioStyle = [i|aspect-ratio: #{ratioW} / #{ratioH}|]
      ratioText :: Text
      ratioText = [i|#{ratioW}:#{ratioH}|]
      ratioDecimal :: Text
      ratioDecimal = [i|#{(fromIntegral ratioW :: Double) / (fromIntegral ratioH :: Double)}|]
      friendlyTypes = humanReadableTypes accept

  -- Alpine.js state for this picker
  Lucid.div_
    [ xData_
        [i|{
  fileName: '',
  fileSize: 0,
  previewUrl: '',
  currentCleared: false,
  isValid: true,
  error: '',
  isDragging: false,

  // Cropper state
  showCropper: false,
  cropper: null,
  originalFile: null,
  cropImageUrl: '',

  handleFileChange(event) {
    const file = event.target.files?.[0];
    if (file && file.type.startsWith('image/')) {
      this.openCropper(file);
    }
  },

  handleDrop(event) {
    this.isDragging = false;
    const file = event.dataTransfer?.files?.[0];
    if (file && file.type.startsWith('image/')) {
      this.openCropper(file);
    }
  },

  openCropper(file) {
    // Validate file size first
    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      this.isValid = false;
      this.error = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }

    this.originalFile = file;
    this.cropImageUrl = URL.createObjectURL(file);
    this.showCropper = true;

    // Initialize cropper after DOM updates
    this.$nextTick(() => {
      const img = this.$refs.cropperImage;
      if (img && typeof Cropper !== 'undefined') {
        this.cropper = new Cropper(img, {
          aspectRatio: #{ratioDecimal},
          viewMode: 1,
          dragMode: 'move',
          autoCropArea: 1,
          restore: false,
          guides: true,
          center: true,
          highlight: false,
          cropBoxMovable: true,
          cropBoxResizable: true,
          toggleDragModeOnDblclick: false,
        });
      }
    });
  },

  async confirmCrop() {
    if (!this.cropper) return;

    const canvas = this.cropper.getCroppedCanvas({
      maxWidth: 2048,
      maxHeight: 2048,
      imageSmoothingEnabled: true,
      imageSmoothingQuality: 'high',
    });

    // Convert canvas to blob
    const blob = await new Promise(resolve => {
      canvas.toBlob(resolve, 'image/jpeg', 0.92);
    });

    // Create a new File from the blob
    const croppedFile = new File([blob], this.originalFile.name, {
      type: 'image/jpeg',
      lastModified: Date.now(),
    });

    // Update the file input
    const input = document.getElementById('#{inputId}');
    const dt = new DataTransfer();
    dt.items.add(croppedFile);
    input.files = dt.files;

    // Update preview
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
    }
    this.previewUrl = URL.createObjectURL(croppedFile);
    this.fileName = croppedFile.name;
    this.fileSize = croppedFile.size;
    this.currentCleared = false;
    this.isValid = true;
    this.error = '';

    this.closeCropper();
  },

  cancelCrop() {
    // Clear the file input
    const input = document.getElementById('#{inputId}');
    if (input) input.value = '';
    this.closeCropper();
  },

  closeCropper() {
    if (this.cropper) {
      this.cropper.destroy();
      this.cropper = null;
    }
    if (this.cropImageUrl) {
      URL.revokeObjectURL(this.cropImageUrl);
      this.cropImageUrl = '';
    }
    this.showCropper = false;
    this.originalFile = null;
  },

  clearFile() {
    const input = document.getElementById('#{inputId}');
    if (input) input.value = '';
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
    }
    this.fileName = '';
    this.fileSize = 0;
    this.previewUrl = '';
    this.currentCleared = true;
    this.validate();
  },

  validate() {
    if (#{if isRequired then "true" else "false" :: Text} && this.currentCleared && !this.previewUrl) {
      this.isValid = false;
      this.error = 'This field is required';
      return;
    }
    this.isValid = true;
    this.error = '';
  },

  triggerInput() {
    document.getElementById('#{inputId}').click();
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 10) / 10 + ' ' + sizes[i];
  }
}|]
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          class_ $ base [Tokens.fontBold, Tokens.textSm, "uppercase", "tracking-wide", Tokens.mb2, "block"]
        ]
        (Lucid.toHtml labelText)

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ fieldName,
          Lucid.id_ inputId,
          Lucid.accept_ accept,
          Lucid.class_ "hidden",
          xOnChange_ "handleFileChange($event)"
        ]
          <> [Lucid.required_ "" | isRequired]

      -- Drop zone container (entire area is clickable)
      Lucid.div_
        [ xBindClass_
            [i|{
            'border-2 border-dashed p-6 cursor-pointer transition-all duration-150': true,
            'border-red-500 bg-red-50': !isValid,
            'border-purple-500 bg-purple-50': isDragging && isValid,
            'border-gray-300 hover:border-gray-400 hover:bg-gray-50': !isDragging && isValid
          }|],
          xOnClick_ "triggerInput()",
          xOnDragover_ "isDragging = true",
          xOnDragleave_ "isDragging = false",
          xOnDrop_ "handleDrop($event)"
        ]
        $ do
          -- Preview area
          Lucid.div_ [class_ $ base ["flex", "flex-col", "items-center"]] $ do
            -- Empty/placeholder state
            let emptyCondition =
                  if hasExisting
                    then "currentCleared && !previewUrl"
                    else "!previewUrl"
                emptyStyle = if hasExisting then "display: none;" else ""

            Lucid.div_ [xShow_ emptyCondition, Lucid.style_ emptyStyle, class_ $ base ["flex", "flex-col", "items-center"]] $ do
              -- Aspect ratio preview box
              Lucid.div_
                [ class_ $ base ["w-40", "border-2", "border-dashed", "border-gray-300", "bg-white", "flex", "items-center", "justify-center", "mb-4", "relative", "overflow-hidden"],
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  -- Decorative corner markers to indicate crop area
                  Lucid.div_ [Lucid.class_ "absolute top-0 left-0 w-3 h-3 border-t-2 border-l-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute top-0 right-0 w-3 h-3 border-t-2 border-r-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute bottom-0 left-0 w-3 h-3 border-b-2 border-l-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute bottom-0 right-0 w-3 h-3 border-b-2 border-r-2 border-gray-400"] mempty
                  -- Center icon
                  Lucid.div_ [class_ $ base ["text-gray-300", "text-3xl"]] $
                    Lucid.toHtmlRaw ("&#x1F5BC;" :: Text) -- framed picture icon

              -- Call to action
              Lucid.div_ [class_ $ base ["text-center"]] $ do
                Lucid.p_ [class_ $ base [Tokens.fontBold, "text-gray-700", "mb-1"]] $
                  Lucid.span_ [xShow_ "!isDragging"] "Drop image here or click to browse"
                Lucid.p_ [class_ $ base [Tokens.fontBold, "text-purple-600", "mb-1"], xShow_ "isDragging", Lucid.style_ "display: none;"] "Drop to upload"
                -- Requirements in a subtle format
                Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] $ do
                  Lucid.toHtml ([i|#{ratioText}|] :: Text)
                  Lucid.span_ [class_ $ base ["mx-2", "text-gray-300"]] "·"
                  Lucid.toHtml ([i|Max #{maxSizeMB}MB|] :: Text)
                  Lucid.span_ [class_ $ base ["mx-2", "text-gray-300"]] "·"
                  Lucid.toHtml friendlyTypes

            -- Current/existing image
            if hasExisting
              then Lucid.div_ [xShow_ "!currentCleared && !previewUrl", class_ $ base ["flex", "flex-col", "items-center"]] $ do
                Lucid.div_
                  [ class_ $ base ["w-40", "border-2", "border-gray-300", "bg-gray-100", "mb-3", "relative", "group", "overflow-hidden"],
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ Lucid.src_ existingImageUrl,
                        Lucid.class_ "w-full h-full object-cover",
                        Lucid.alt_ "Current image"
                      ]
                    -- Hover overlay
                    Lucid.div_ [Lucid.class_ "absolute inset-0 bg-black/50 opacity-0 group-hover:opacity-100 transition-opacity flex items-center justify-center"] $
                      Lucid.span_ [Lucid.class_ "text-white text-sm font-bold"] "Click to change"
                Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb2]] "Current image"
                -- Clear button (stop propagation to prevent triggering file input)
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearFile()",
                    class_ $ base [Tokens.textSm, "text-red-600", "hover:text-red-800", "underline"]
                  ]
                  "Remove image"
              else mempty

            -- New file preview
            Lucid.div_ [xShow_ "previewUrl", Lucid.style_ "display: none;", class_ $ base ["flex", "flex-col", "items-center"]] $ do
              Lucid.div_
                [ class_ $ base ["w-40", "border-2", "border-purple-400", "bg-gray-100", "mb-3", "relative", "group", "overflow-hidden"],
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  Lucid.img_
                    [ xBindSrc_ "previewUrl",
                      Lucid.class_ "w-full h-full object-cover",
                      Lucid.alt_ "New image preview"
                    ]
                  -- Hover overlay
                  Lucid.div_ [Lucid.class_ "absolute inset-0 bg-black/50 opacity-0 group-hover:opacity-100 transition-opacity flex items-center justify-center"] $
                    Lucid.span_ [Lucid.class_ "text-white text-sm font-bold"] "Click to change"

              -- File info chip
              Lucid.div_ [class_ $ base ["flex", "items-center", "gap-2", "px-3", "py-1.5", "bg-purple-100", "border", "border-purple-300", "text-purple-800", Tokens.textSm, "mb-2"]] $ do
                Lucid.span_ [class_ $ base [Tokens.fontBold], xText_ "fileName"] ""
                Lucid.span_ [class_ $ base ["text-purple-600"]] $ do
                  "("
                  Lucid.span_ [xText_ "formatFileSize(fileSize)"] ""
                  ")"
              -- Clear button
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "$event.stopPropagation(); clearFile()",
                  class_ $ base [Tokens.textSm, "text-red-600", "hover:text-red-800", "underline"]
                ]
                "Remove"

      -- Error message
      Lucid.div_
        [ xShow_ "!isValid",
          class_ $ base ["mt-2", "flex", "items-center", "gap-2", Tokens.textSm, "text-red-600"],
          Lucid.style_ "display: none;"
        ]
        $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold]] "!"
          Lucid.span_ [xText_ "error"] ""

      -- Optional hint for non-required fields with existing images
      if not isRequired && hasExisting
        then
          Lucid.p_ [class_ $ base ["mt-2", Tokens.textSm, Tokens.textGray600]] $
            "Leave unchanged to keep the current image"
        else mempty

      -- Cropper Modal
      Lucid.div_
        [ xShow_ "showCropper",
          Lucid.style_ "display: none;",
          class_ $ base ["fixed", "inset-0", "z-50", "flex", "items-center", "justify-center", "p-4"]
        ]
        $ do
          -- Backdrop
          Lucid.div_
            [ xOnClick_ "cancelCrop()",
              class_ $ base ["absolute", "inset-0", "bg-black/70"]
            ]
            mempty

          -- Modal content
          Lucid.div_ [class_ $ base ["relative", "bg-white", "max-w-2xl", "w-full", "max-h-[90vh]", "flex", "flex-col", "shadow-xl"]] $ do
            -- Header
            Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", "px-4", "py-3", "border-b", "border-gray-200"]] $ do
              Lucid.h3_ [class_ $ base [Tokens.fontBold, "text-gray-800"]] $ do
                "Crop Image"
                Lucid.span_ [class_ $ base ["font-normal", Tokens.textSm, Tokens.textGray600, "ml-2"]] $
                  Lucid.toHtml ([i|(#{ratioText} ratio)|] :: Text)
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "cancelCrop()",
                  class_ $ base ["text-gray-400", "hover:text-gray-600", "text-xl", Tokens.fontBold]
                ]
                "×"

            -- Cropper container
            Lucid.div_ [class_ $ base ["flex-1", "overflow-hidden", "bg-gray-900", "min-h-[300px]", "max-h-[60vh]"]] $
              Lucid.img_
                [ xRef_ "cropperImage",
                  xBindSrc_ "cropImageUrl",
                  Lucid.class_ "max-w-full",
                  Lucid.alt_ "Image to crop"
                ]

            -- Footer with actions
            Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", "px-4", "py-3", "border-t", "border-gray-200", "bg-gray-50"]] $ do
              -- Zoom controls
              Lucid.div_ [class_ $ base ["flex", "items-center", "gap-2"]] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(-0.1)",
                    class_ $ base ["px-3", "py-1", "bg-gray-200", "hover:bg-gray-300", Tokens.textSm, Tokens.fontBold]
                  ]
                  "−"
                Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] "Zoom"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(0.1)",
                    class_ $ base ["px-3", "py-1", "bg-gray-200", "hover:bg-gray-300", Tokens.textSm, Tokens.fontBold]
                  ]
                  "+"

              -- Action buttons
              Lucid.div_ [class_ $ base ["flex", "items-center", "gap-3"]] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cancelCrop()",
                    class_ $ base ["px-4", "py-2", "bg-gray-300", "hover:bg-gray-400", Tokens.fontBold, Tokens.textSm]
                  ]
                  "Cancel"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "confirmCrop()",
                    class_ $ base ["px-4", "py-2", "bg-purple-600", "hover:bg-purple-700", Tokens.textWhite, Tokens.fontBold, Tokens.textSm]
                  ]
                  "Apply Crop"
