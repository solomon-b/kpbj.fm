-- | Form styling configuration.
--
-- This module provides customizable CSS classes for form elements.
-- The 'defaultFormStyles' works well with Tailwind CSS.
module Component.Form.Builder.Styles
  ( FormStyles (..),
    defaultFormStyles,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)

--------------------------------------------------------------------------------

-- | CSS class configuration for form elements.
--
-- Each field contains Tailwind CSS classes (or any CSS framework classes)
-- that are applied to the corresponding form elements.
data FormStyles = FormStyles
  { -- | Text input (normal state)
    fsTextInputClasses :: Text,
    -- | Text input (error state)
    fsTextInputErrorClasses :: Text,
    -- | Text input (disabled state)
    fsTextInputDisabledClasses :: Text,
    -- | Textarea (normal state)
    fsTextareaClasses :: Text,
    -- | Textarea (error state)
    fsTextareaErrorClasses :: Text,
    -- | Textarea (disabled state)
    fsTextareaDisabledClasses :: Text,
    -- | Select dropdown
    fsSelectClasses :: Text,
    -- | Select dropdown (error state)
    fsSelectErrorClasses :: Text,
    -- | Select dropdown (disabled state)
    fsSelectDisabledClasses :: Text,
    -- | File upload zone (normal state)
    fsFileUploadClasses :: Text,
    -- | File upload zone (error state)
    fsFileUploadErrorClasses :: Text,
    -- | Radio button group container
    fsRadioGroupClasses :: Text,
    -- | Radio button group (error state)
    fsRadioGroupErrorClasses :: Text,
    -- | Individual radio button label
    fsRadioLabelClasses :: Text,
    -- | Checkbox label
    fsCheckboxLabelClasses :: Text,
    -- | Field label
    fsLabelClasses :: Text,
    -- | Error message text
    fsErrorMessageClasses :: Text,
    -- | Hint text
    fsHintClasses :: Text,
    -- | Section container
    fsSectionContainerClasses :: Text,
    -- | Section title
    fsSectionTitleClasses :: Text,
    -- | Section content container
    fsSectionContentClasses :: Text,
    -- | Form container
    fsFormClasses :: Text,
    -- | Form title (h1)
    fsTitleClasses :: Text,
    -- | Form subtitle
    fsSubtitleClasses :: Text,
    -- | Button container
    fsButtonContainerClasses :: Text,
    -- | Submit button
    fsSubmitButtonClasses :: Text,
    -- | Cancel link/button
    fsCancelButtonClasses :: Text,
    -- Image picker styles

    -- | Image drop zone (normal state)
    fsImageDropZoneClasses :: Text,
    -- | Image drop zone (dragging state)
    fsImageDropZoneDraggingClasses :: Text,
    -- | Image drop zone (error state)
    fsImageDropZoneErrorClasses :: Text,
    -- | Image preview container
    fsImagePreviewClasses :: Text,
    -- | Image picker action button (e.g., "Remove")
    fsImageActionButtonClasses :: Text,
    -- Audio picker styles

    -- | Audio picker outer container
    fsAudioContainerClasses :: Text,
    -- | Audio picker file select button
    fsAudioButtonClasses :: Text,
    -- Toggle switch styles

    -- | Toggle container
    fsToggleContainerClasses :: Text,
    -- | Toggle off label
    fsToggleOffLabelClasses :: Text,
    -- | Toggle on label
    fsToggleOnLabelClasses :: Text,
    -- | Toggle switch wrapper (the label containing the input)
    fsToggleSwitchClasses :: Text,
    -- | Toggle switch wrapper (disabled state)
    fsToggleSwitchDisabledClasses :: Text,
    -- | Toggle track (the background pill)
    fsToggleTrackClasses :: Text,
    -- | Toggle track (checked state)
    fsToggleTrackCheckedClasses :: Text,
    -- | Toggle track (disabled state)
    fsToggleTrackDisabledClasses :: Text
  }
  deriving stock (Show)

-- | Default form styles using Tailwind CSS classes.
--
-- These provide a clean, functional design that works well
-- with the KPBJ brutalist aesthetic.
defaultFormStyles :: FormStyles
defaultFormStyles =
  FormStyles
    { fsTextInputClasses =
        "w-full p-3 border-2 border-gray-400 dark:border-gray-500 bg-white dark:bg-gray-800 font-mono focus:border-blue-600 focus:outline-none",
      fsTextInputErrorClasses =
        "w-full p-3 border-2 border-red-500 bg-white dark:bg-gray-800 font-mono focus:border-red-600 focus:outline-none",
      fsTextInputDisabledClasses =
        "w-full p-3 border-2 border-gray-300 dark:border-gray-600 bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-400 font-mono cursor-not-allowed",
      fsTextareaClasses =
        "w-full p-3 border-2 border-gray-400 dark:border-gray-500 bg-white dark:bg-gray-800 font-mono leading-relaxed focus:border-blue-600 focus:outline-none",
      fsTextareaErrorClasses =
        "w-full p-3 border-2 border-red-500 bg-white dark:bg-gray-800 font-mono leading-relaxed focus:border-red-600 focus:outline-none",
      fsTextareaDisabledClasses =
        "w-full p-3 border-2 border-gray-300 dark:border-gray-600 bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-400 font-mono leading-relaxed cursor-not-allowed",
      fsSelectClasses =
        "w-full p-3 border-2 border-gray-400 dark:border-gray-500 bg-white dark:bg-gray-800 font-mono focus:border-blue-600 focus:outline-none",
      fsSelectErrorClasses =
        "w-full p-3 border-2 border-red-500 bg-white dark:bg-gray-800 font-mono focus:border-red-600 focus:outline-none",
      fsSelectDisabledClasses =
        "w-full p-3 border-2 border-gray-300 dark:border-gray-600 bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-400 font-mono cursor-not-allowed",
      fsFileUploadClasses =
        "border-2 border-dashed border-gray-400 dark:border-gray-500 p-6 text-center cursor-pointer hover:border-gray-600 dark:hover:border-gray-400 transition-colors",
      fsFileUploadErrorClasses =
        "border-2 border-dashed border-red-500 p-6 text-center cursor-pointer",
      fsRadioGroupClasses =
        "space-y-2",
      fsRadioGroupErrorClasses =
        "space-y-2 p-2 border-2 border-red-500 -m-2",
      fsRadioLabelClasses =
        "flex items-center gap-2 cursor-pointer",
      fsCheckboxLabelClasses =
        "flex items-center gap-2 cursor-pointer",
      fsLabelClasses =
        "block font-bold mb-2",
      fsErrorMessageClasses =
        "mt-1 text-sm text-red-600 dark:text-red-400",
      fsHintClasses =
        "text-xs text-gray-600 dark:text-gray-400 mt-1",
      fsSectionContainerClasses =
        "bg-white dark:bg-gray-800 p-6",
      fsSectionTitleClasses =
        "text-xl font-bold mb-4 pb-2",
      fsSectionContentClasses =
        "space-y-6",
      fsFormClasses =
        "space-y-8 w-full",
      fsTitleClasses =
        "text-2xl font-bold",
      fsSubtitleClasses =
        "text-gray-600 dark:text-gray-400 mt-2",
      fsButtonContainerClasses =
        "flex gap-4 justify-end",
      fsSubmitButtonClasses =
        "bg-gray-800 dark:bg-gray-700 text-white font-bold py-3 px-6 hover:bg-gray-700 dark:hover:bg-gray-600 transition-colors",
      fsCancelButtonClasses =
        "bg-gray-300 dark:bg-gray-600 text-gray-800 dark:text-gray-200 font-bold py-3 px-6 hover:bg-gray-400 dark:hover:bg-gray-500 transition-colors",
      -- Image picker defaults
      fsImageDropZoneClasses =
        "border-2 border-dashed border-gray-300 dark:border-gray-600 p-6 cursor-pointer transition-all duration-150 hover:border-gray-400 dark:hover:border-gray-500 hover:bg-gray-50 dark:hover:bg-gray-700",
      fsImageDropZoneDraggingClasses =
        "border-2 border-dashed border-purple-500 bg-purple-50 dark:bg-purple-900 p-6 cursor-pointer transition-all duration-150",
      fsImageDropZoneErrorClasses =
        "border-2 border-dashed border-red-500 bg-red-50 dark:bg-red-900 p-6 cursor-pointer transition-all duration-150",
      fsImagePreviewClasses =
        "border-2 border-gray-300 dark:border-gray-600 bg-gray-100 dark:bg-gray-700 relative group overflow-hidden",
      fsImageActionButtonClasses =
        "text-sm text-red-600 dark:text-red-400 hover:text-red-800 dark:hover:text-red-300 underline",
      -- Audio picker defaults
      fsAudioContainerClasses =
        "border-2 border-dashed border-gray-400 dark:border-gray-500 p-6 text-center",
      fsAudioButtonClasses =
        "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block",
      -- Toggle switch defaults
      fsToggleContainerClasses =
        "flex items-center gap-3",
      fsToggleOffLabelClasses =
        "text-sm font-bold text-gray-600",
      fsToggleOnLabelClasses =
        "text-sm font-bold text-gray-600",
      fsToggleSwitchClasses =
        "relative inline-flex items-center cursor-pointer",
      fsToggleSwitchDisabledClasses =
        "relative inline-flex items-center cursor-not-allowed opacity-50",
      fsToggleTrackClasses =
        "w-11 h-6 bg-gray-300 peer-focus:outline-none peer-focus:ring-2 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-green-600",
      fsToggleTrackCheckedClasses =
        "w-11 h-6 bg-green-600 peer-focus:outline-none peer-focus:ring-2 peer-focus:ring-blue-300 rounded-full peer after:translate-x-full after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all",
      fsToggleTrackDisabledClasses =
        "w-11 h-6 bg-gray-300 rounded-full peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-gray-400"
    }
