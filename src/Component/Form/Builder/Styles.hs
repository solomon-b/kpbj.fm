-- | Form styling configuration.
--
-- This module provides customizable CSS classes for form elements.
-- The 'defaultFormStyles' works well with Tailwind CSS and uses
-- theme-aware tokens from Design.Theme.
module Component.Form.Builder.Styles
  ( FormStyles (..),
    defaultFormStyles,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design.Theme qualified as Theme

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
        "w-full p-3 border " <> Theme.borderDefault <> " " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono focus:border-[var(--theme-info)] focus:outline-none",
      fsTextInputErrorClasses =
        "w-full p-3 border border-[var(--theme-error)] " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono focus:border-[var(--theme-error)] focus:outline-none",
      fsTextInputDisabledClasses =
        "w-full p-3 border " <> Theme.borderMuted <> " " <> Theme.bgAlt <> " " <> Theme.fgMuted <> " font-mono cursor-not-allowed",
      fsTextareaClasses =
        "w-full p-3 border " <> Theme.borderDefault <> " " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono leading-relaxed focus:border-[var(--theme-info)] focus:outline-none",
      fsTextareaErrorClasses =
        "w-full p-3 border border-[var(--theme-error)] " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono leading-relaxed focus:border-[var(--theme-error)] focus:outline-none",
      fsTextareaDisabledClasses =
        "w-full p-3 border " <> Theme.borderMuted <> " " <> Theme.bgAlt <> " " <> Theme.fgMuted <> " font-mono leading-relaxed cursor-not-allowed",
      fsSelectClasses =
        "w-full p-3 border " <> Theme.borderDefault <> " " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono focus:border-[var(--theme-info)] focus:outline-none",
      fsSelectErrorClasses =
        "w-full p-3 border border-[var(--theme-error)] " <> Theme.bgMain <> " " <> Theme.fgPrimary <> " font-mono focus:border-[var(--theme-error)] focus:outline-none",
      fsSelectDisabledClasses =
        "w-full p-3 border " <> Theme.borderMuted <> " " <> Theme.bgAlt <> " " <> Theme.fgMuted <> " font-mono cursor-not-allowed",
      fsFileUploadClasses =
        "border border-dashed " <> Theme.borderDefault <> " p-6 text-center cursor-pointer " <> Theme.hoverBg <> " transition-colors",
      fsFileUploadErrorClasses =
        "border border-dashed border-[var(--theme-error)] p-6 text-center cursor-pointer",
      fsRadioGroupClasses =
        "space-y-2",
      fsRadioGroupErrorClasses =
        "space-y-2 p-2 border border-[var(--theme-error)] -m-2",
      fsRadioLabelClasses =
        "flex items-center gap-2 cursor-pointer " <> Theme.fgPrimary,
      fsCheckboxLabelClasses =
        "flex items-center gap-2 cursor-pointer " <> Theme.fgPrimary,
      fsLabelClasses =
        "block font-bold mb-2 " <> Theme.fgPrimary,
      fsErrorMessageClasses =
        "mt-1 text-sm " <> Theme.error,
      fsHintClasses =
        "text-xs " <> Theme.fgMuted <> " mt-1",
      fsSectionContainerClasses =
        Theme.bgAlt <> " rounded p-6",
      fsSectionTitleClasses =
        "text-xl font-bold mb-4 pb-2 " <> Theme.fgPrimary,
      fsSectionContentClasses =
        "space-y-6",
      fsFormClasses =
        "space-y-8 w-full",
      fsTitleClasses =
        "text-2xl font-bold " <> Theme.fgPrimary,
      fsSubtitleClasses =
        Theme.fgMuted <> " mt-2",
      fsButtonContainerClasses =
        "flex gap-4 justify-end",
      fsSubmitButtonClasses =
        Theme.accent <> " " <> Theme.accentFg <> " font-bold py-3 px-6 " <> Theme.accentHover <> " transition-colors",
      fsCancelButtonClasses =
        Theme.bgAlt <> " " <> Theme.fgPrimary <> " font-bold py-3 px-6 " <> Theme.hoverBg <> " transition-colors",
      -- Image picker defaults
      fsImageDropZoneClasses =
        "border border-dashed " <> Theme.borderDefault <> " p-6 cursor-pointer transition-all duration-150 " <> Theme.hoverBg,
      fsImageDropZoneDraggingClasses =
        "border border-dashed border-[var(--theme-info)] bg-[var(--theme-info)]/10 p-6 cursor-pointer transition-all duration-150",
      fsImageDropZoneErrorClasses =
        "border border-dashed border-[var(--theme-error)] bg-[var(--theme-error)]/10 p-6 cursor-pointer transition-all duration-150",
      fsImagePreviewClasses =
        "border " <> Theme.borderDefault <> " " <> Theme.bgAlt <> " relative group overflow-hidden",
      fsImageActionButtonClasses =
        "text-sm " <> Theme.error <> " hover:opacity-80 underline",
      -- Audio picker defaults
      fsAudioContainerClasses =
        "border border-dashed " <> Theme.borderDefault <> " p-6 text-center",
      fsAudioButtonClasses =
        "bg-[var(--theme-info)] " <> Theme.fgInverse <> " px-6 py-3 font-bold hover:opacity-90 inline-block",
      -- Toggle switch defaults
      fsToggleContainerClasses =
        "flex items-center gap-3",
      fsToggleOffLabelClasses =
        "text-sm font-bold " <> Theme.fgMuted,
      fsToggleOnLabelClasses =
        "text-sm font-bold " <> Theme.fgMuted,
      fsToggleSwitchClasses =
        "relative inline-flex items-center cursor-pointer",
      fsToggleSwitchDisabledClasses =
        "relative inline-flex items-center cursor-not-allowed opacity-50",
      fsToggleTrackClasses =
        "w-11 h-6 bg-gray-300 peer-focus:outline-none peer-focus:ring-2 peer-focus:ring-[var(--theme-info)] rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-[var(--theme-success)]",
      fsToggleTrackCheckedClasses =
        "w-11 h-6 bg-[var(--theme-success)] peer-focus:outline-none peer-focus:ring-2 peer-focus:ring-[var(--theme-info)] rounded-full peer after:translate-x-full after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all",
      fsToggleTrackDisabledClasses =
        "w-11 h-6 bg-gray-300 rounded-full peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-gray-400"
    }
