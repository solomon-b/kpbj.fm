-- | Form Builder: Writer Monad-Based Form Builder
--
-- This module provides a declarative API for building forms using @do@ notation,
-- inspired by the 'Design.StyleBuilder' pattern.
--
-- = Example Usage
--
-- > import Component.Form.Builder
-- >
-- > myForm :: Lucid.Html ()
-- > myForm = renderForm config formBuilder
-- >   where
-- >     config = defaultFormConfig
-- >       { fcAction = "/submit"
-- >       , fcHtmxTarget = Just "#main-content"
-- >       }
-- >
-- >     formBuilder = do
-- >       hidden "csrf_token" token
-- >
-- >       section "User Details" do
-- >         textField "name" do
-- >           label "Full Name"
-- >           placeholder "Enter your name"
-- >           required
-- >           minLength 2
-- >
-- >         textField "email" do
-- >           label "Email Address"
-- >           required
-- >           pattern "[^@]+@[^@]+"
-- >
-- >       section "Profile" do
-- >         textareaField "bio" 4 do
-- >           label "Biography"
-- >           maxLength 500
-- >           hint "Tell us about yourself"
-- >
-- >         fileField "avatar" (Just "image/*") do
-- >           label "Profile Picture"
-- >           maxSize 5
--
-- = Key Features
--
-- * __Monadic API__: Build forms with @do@ notation
-- * __Type-safe validation__: Composable validation rules
-- * __Alpine.js integration__: Automatic client-side validation
-- * __HTMX support__: Progressive enhancement
-- * __Full field types__: Text, textarea, select, radio, file, datetime, number, checkbox
module Component.Form.Builder
  ( -- * Form Building
    FormBuilder,
    renderForm,

    -- * Form Configuration
    FormConfig (..),
    defaultFormConfig,

    -- * Field Functions
    textField,
    passwordField,
    textareaField,
    selectField,
    radioField,
    fileField,
    imageField,
    audioField,
    dateTimeField,
    numberField,
    checkboxField,
    toggleField,

    -- * Hidden Fields
    hidden,

    -- * Form Metadata
    formTitle,
    formSubtitle,

    -- * Structure
    section,
    conditional,
    plain,

    -- * Footer Items (Buttons and Inline Controls)
    submitButton,
    cancelButton,
    footerToggle,
    footerHint,

    -- * Field Configuration
    FieldBuilder,
    label,
    placeholder,
    hint,
    description,
    value,
    disabled,
    classes,

    -- * File Field Configuration
    maxSize,
    buttonText,
    currentFile,
    aspectRatio,

    -- * Toggle Field Configuration
    onLabel,
    offLabel,
    onValue,
    offValue,
    checked,

    -- * Select/Radio Options
    addOption,
    addOptionSelected,
    addOptionWithDesc,
    addOptionSelectedWithDesc,

    -- * Validation
    required,
    minLength,
    maxLength,
    pattern,
    customValidation,

    -- * Conditional Building
    when,
    unless,

    -- * Types (re-exports from Types)
    SelectOption (..),
    FormFooterItem (..),

    -- * Styles
    FormStyles (..),
    defaultFormStyles,

    -- * JS Expressions (for advanced custom validation)
    JSExpr,
    JS.str,
    JS.num,
    JS.int,
    JS.bool,
    JS.var,
    JS.field,
    JS.method,
    JS.raw,
    (JS..&&),
    (JS..||),
    (JS..==),
    (JS..!=),
    (JS..<),
    (JS..<=),
    (JS..>),
    (JS..>=),
    JS.not_,
    JS.checkRequired,
    JS.checkMinLength,
    JS.checkMaxLength,
    JS.checkPattern,
    JS.allOf,
    JS.anyOf,
    JS.renderExpr,
  )
where

--------------------------------------------------------------------------------

import Component.Form.Builder.Core
  ( FormBuilder,
    audioField,
    cancelButton,
    checkboxField,
    conditional,
    dateTimeField,
    fileField,
    footerHint,
    footerToggle,
    formSubtitle,
    formTitle,
    hidden,
    imageField,
    numberField,
    passwordField,
    plain,
    radioField,
    section,
    selectField,
    submitButton,
    textField,
    textareaField,
    toggleField,
  )
import Component.Form.Builder.Field
  ( FieldBuilder,
    addOption,
    addOptionSelected,
    addOptionSelectedWithDesc,
    addOptionWithDesc,
    aspectRatio,
    buttonText,
    checked,
    classes,
    currentFile,
    customValidation,
    description,
    disabled,
    hint,
    label,
    maxLength,
    maxSize,
    minLength,
    offLabel,
    offValue,
    onLabel,
    onValue,
    pattern,
    placeholder,
    required,
    value,
  )
import Component.Form.Builder.JS (JSExpr)
import Component.Form.Builder.JS qualified as JS
import Component.Form.Builder.Render
  ( FormConfig (..),
    defaultFormConfig,
    renderForm,
  )
import Component.Form.Builder.Styles (FormStyles (..), defaultFormStyles)
import Component.Form.Builder.Types (FormFooterItem (..), SelectOption (..))
import Control.Monad (unless, when)
