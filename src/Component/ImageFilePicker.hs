{-# LANGUAGE QuasiQuotes #-}

module Component.ImageFilePicker
  ( Config (..),
    render,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras (xBindClass_, xBindSrc_, xData_, xOnChange_, xOnClick_, xShow_, xText_)

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
    isRequired :: Bool
  }

--------------------------------------------------------------------------------

-- | Render an image file picker with integrated preview.
--
-- Combines a file input and image preview in one dashed border box.
-- The preview shows the existing image (if any) or updates when a new file is selected.
render :: Config -> Lucid.Html ()
render Config {..} = do
  let labelText :: Text
      labelText = if isRequired then label <> " *" else label
      inputId = fieldName <> "-input"
      hasExisting = not (existingImageUrl == "")

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

  handleFileChange(event) {
    const file = event.target.files?.[0];
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
      this.previewUrl = '';
    }
    if (file) {
      this.fileName = file.name;
      this.fileSize = file.size;
      if (file.type.startsWith('image/')) {
        this.previewUrl = URL.createObjectURL(file);
      }
      this.validate(file);
    } else {
      this.fileName = '';
      this.fileSize = 0;
    }
  },

  validate(file) {
    if (!file && #{if isRequired then "true" else "false" :: Text} && this.currentCleared) {
      this.isValid = false;
      this.error = 'This field is required';
      return;
    }
    if (file && file.size > #{maxSizeMB * 1024 * 1024}) {
      this.isValid = false;
      this.error = 'File size must be less than #{maxSizeMB}MB';
      return;
    }
    this.isValid = true;
    this.error = '';
  },

  clearFile() {
    const input = document.getElementById('#{inputId}');
    if (input) {
      input.value = '';
      input.dispatchEvent(new Event('change', { bubbles: true }));
    }
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
    }
    this.fileName = '';
    this.fileSize = 0;
    this.previewUrl = '';
    this.currentCleared = true;
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 Bytes';
    const k = 1024;
    const sizes = ['Bytes', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
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

      -- Dashed border container with preview and file picker
      Lucid.div_
        [ xBindClass_ [i|!isValid ? 'border-2 border-dashed border-red-500 p-6 text-center' : 'border-2 border-dashed border-gray-400 p-6 text-center'|]
        ]
        $ do
          -- Image preview area
          Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
            -- Current/existing image (shown when not cleared and no new file)
            if hasExisting
              then
                Lucid.div_ [xShow_ "!currentCleared && !previewUrl", class_ $ base ["flex", "flex-col", "items-center"]] $ do
                  Lucid.img_
                    [ Lucid.src_ existingImageUrl,
                      Lucid.class_ "max-w-xs max-h-48 border-2 border-gray-400 object-contain mb-2",
                      Lucid.alt_ "Current image"
                    ]
                  Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] "Current image"
              else
                Lucid.div_ [xShow_ "!previewUrl", class_ $ base ["py-8", Tokens.textGray600]] $ do
                  Lucid.div_ [class_ $ base ["text-4xl", Tokens.mb2]] "🖼️"
                  Lucid.p_ [class_ $ base [Tokens.textSm]] "No image selected"

            -- New file preview (shown when a new file is selected)
            Lucid.div_ [xShow_ "previewUrl", Lucid.style_ "display: none;", class_ $ base ["flex", "flex-col", "items-center"]] $ do
              Lucid.img_
                [ xBindSrc_ "previewUrl",
                  Lucid.class_ "max-w-xs max-h-48 border-2 border-gray-400 object-contain mb-2",
                  Lucid.alt_ "New image preview"
                ]
              Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] "New image selected"

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

          -- File picker button and info
          Lucid.label_ [Lucid.for_ inputId, Lucid.class_ "cursor-pointer"] $ do
            Lucid.div_ [class_ $ base ["bg-purple-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-purple-700", "inline-block"]] $
              "CHOOSE IMAGE"
            Lucid.div_ [class_ $ base ["mt-2", Tokens.textSm, Tokens.textGray600]] $
              if isRequired
                then Lucid.toHtml ([i|#{accept} accepted • Max #{maxSizeMB}MB|] :: Text)
                else Lucid.toHtml ([i|#{accept} accepted • Max #{maxSizeMB}MB • Leave empty to keep current|] :: Text)

          -- Selected file info
          Lucid.div_ [xShow_ "fileName", Lucid.class_ "mt-3 text-sm font-bold text-gray-800 flex items-center justify-center gap-2 flex-wrap", Lucid.style_ "display: none;"] $ do
            Lucid.span_ [xText_ "fileName"] ""
            Lucid.span_ [Lucid.class_ "text-gray-600"] $ do
              "("
              Lucid.span_ [xText_ "formatFileSize(fileSize)"] ""
              ")"
            Lucid.button_
              [ Lucid.type_ "button",
                xOnClick_ "clearFile()",
                Lucid.class_ "ml-2 px-2 py-0.5 text-xs bg-gray-200 hover:bg-gray-300 border border-gray-400 text-gray-700 font-normal"
              ]
              "Clear"

      -- Error message
      Lucid.div_
        [ xShow_ "!isValid",
          Lucid.class_ "mt-1 text-sm text-red-600",
          xText_ "error",
          Lucid.style_ "display: none;"
        ]
        ""
