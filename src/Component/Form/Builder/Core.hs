-- | Form builder monad and field functions.
--
-- This module provides the main API for building forms using @do@ notation.
-- Forms are built by accumulating fields, sections, and hidden fields into
-- a 'FormState' via the 'FormBuilder' Writer monad.
--
-- Example:
--
-- > myForm :: FormBuilder
-- > myForm = do
-- >   hidden "csrf_token" token
-- >
-- >   section "User Details" do
-- >     textField "name" do
-- >       label "Full Name"
-- >       required
-- >       minLength 2
-- >
-- >     textField "email" do
-- >       label "Email Address"
-- >       required
-- >       pattern "[^@]+@[^@]+"
-- >
-- >   section "Profile" do
-- >     textareaField "bio" 4 do
-- >       label "Biography"
-- >       maxLength 500
-- >       hint "Tell us about yourself"
module Component.Form.Builder.Core
  ( -- * Builder Type
    FormBuilder,
    runFormBuilder,

    -- * Form Metadata
    formTitle,
    formSubtitle,

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

    -- * Structure
    section,
    conditional,
    plain,

    -- * Footer Items (Buttons and Inline Controls)
    submitButton,
    cancelButton,
    footerToggle,
    footerHint,
  )
where

--------------------------------------------------------------------------------

import Component.Form.Builder.Field (FieldBuilder, FieldBuilderState (..), runFieldBuilder)
import Component.Form.Builder.Types
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------
-- Types

-- | The form builder monad.
--
-- A 'Writer' that accumulates 'FormState'.
type FormBuilder = Writer FormState ()

-- | Execute a form builder and extract the accumulated state.
runFormBuilder :: FormBuilder -> FormState
runFormBuilder = execWriter

--------------------------------------------------------------------------------
-- Internal Helpers

-- | Add a field to the current form state.
tellField :: Field -> FormBuilder
tellField f = tell $ emptyFormState {fsElements = [FieldElement f]}

-- | Add a section to the current form state.
tellSection :: Section -> FormBuilder
tellSection s = tell $ emptyFormState {fsElements = [SectionElement s]}

-- | Add a hidden field to the current form state.
tellHidden :: Text -> Text -> FormBuilder
tellHidden name val = tell $ emptyFormState {fsHiddenFields = [(name, val)]}

-- | Set the form title.
tellTitle :: Text -> FormBuilder
tellTitle t = tell $ emptyFormState {fsTitle = Just t}

-- | Set the form subtitle.
tellSubtitle :: Text -> FormBuilder
tellSubtitle t = tell $ emptyFormState {fsSubtitle = Just t}

-- | Add a footer item to the form.
tellFooterItem :: FormFooterItem -> FormBuilder
tellFooterItem item = tell $ emptyFormState {fsFooterItems = [item]}

-- | Set the footer hint.
tellFooterHint :: Text -> FormBuilder
tellFooterHint h = tell $ emptyFormState {fsFooterHint = Just h}

-- | Build a field from name, type, and builder.
buildField :: Text -> FieldType -> FieldBuilder -> Field
buildField name fieldType builder =
  let state = runFieldBuilder builder
   in Field
        { fName = name,
          fType = fieldType,
          fConfig = fbsConfig state,
          fValidation = fbsValidation state
        }

--------------------------------------------------------------------------------
-- Form Metadata

-- | Set the form title (rendered as h1 above the form).
--
-- > formTitle "Edit Profile"
formTitle :: Text -> FormBuilder
formTitle = tellTitle

-- | Set the form subtitle (rendered below the title).
--
-- > formSubtitle "Update your profile information"
formSubtitle :: Text -> FormBuilder
formSubtitle = tellSubtitle

--------------------------------------------------------------------------------
-- Field Functions

-- | Add a single-line text input field.
--
-- > textField "email" do
-- >   label "Email Address"
-- >   placeholder "you@example.com"
-- >   required
textField :: Text -> FieldBuilder -> FormBuilder
textField name builder = tellField $ buildField name TextField builder

-- | Add a password input field (masked text).
--
-- > passwordField "password" do
-- >   label "Password"
-- >   placeholder "Enter your password"
-- >   required
passwordField :: Text -> FieldBuilder -> FormBuilder
passwordField name builder = tellField $ buildField name PasswordField builder

-- | Add a multi-line textarea field.
--
-- > textareaField "description" 4 do
-- >   label "Description"
-- >   maxLength 1000
textareaField :: Text -> Int -> FieldBuilder -> FormBuilder
textareaField name rows builder =
  tellField $ buildField name (TextareaField rows) builder

-- | Add a dropdown select field.
--
-- > selectField "country" do
-- >   label "Country"
-- >   required
-- >   addOption "us" "United States"
-- >   addOption "ca" "Canada"
selectField :: Text -> FieldBuilder -> FormBuilder
selectField name builder =
  tellField $ buildField name SelectField builder

-- | Add a radio button group.
--
-- > radioField "plan" do
-- >   label "Select Plan"
-- >   required
-- >   addOptionWithDesc "free" "Free" "Basic features"
-- >   addOptionWithDesc "pro" "Pro" "All features"
radioField :: Text -> FieldBuilder -> FormBuilder
radioField name builder =
  tellField $ buildField name RadioField builder

-- | Add a generic file upload field.
--
-- > fileField "document" (Just "application/pdf") do
-- >   label "Upload Document"
-- >   maxSize 10
fileField :: Text -> Maybe Text -> FieldBuilder -> FormBuilder
fileField name accept builder =
  tellField $ buildField name (FileField accept) builder

-- | Add an image upload field with preview and optional cropping.
--
-- > imageField "avatar" do
-- >   label "Profile Picture"
-- >   maxSize 5
-- >   aspectRatio (1, 1)
-- >   currentFile existingAvatarUrl
imageField :: Text -> FieldBuilder -> FormBuilder
imageField name builder =
  tellField $ buildField name (ImageField (Just "image/*")) builder

-- | Add an audio upload field with player preview.
--
-- > audioField "episode_audio" do
-- >   label "Episode Audio"
-- >   maxSize 500
-- >   currentFile existingAudioUrl
audioField :: Text -> FieldBuilder -> FormBuilder
audioField name builder =
  tellField $ buildField name (AudioField (Just "audio/*")) builder

-- | Add a date/time picker field.
--
-- > dateTimeField "event_date" do
-- >   label "Event Date"
-- >   required
dateTimeField :: Text -> FieldBuilder -> FormBuilder
dateTimeField name builder =
  tellField $ buildField name DateTimeField builder

-- | Add a numeric input field.
--
-- > numberField "quantity" (Just 1) (Just 100) (Just 1) do
-- >   label "Quantity"
-- >   required
numberField :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> FieldBuilder -> FormBuilder
numberField name minVal maxVal step builder =
  tellField $ buildField name (NumberField minVal maxVal step) builder

-- | Add a checkbox field.
--
-- > checkboxField "terms" do
-- >   label "I agree to the terms"
-- >   required
checkboxField :: Text -> FieldBuilder -> FormBuilder
checkboxField name builder =
  tellField $ buildField name CheckboxField builder

-- | Add a toggle switch field with on/off labels.
--
-- > toggleField "status" do
-- >   offLabel "Draft"
-- >   onLabel "Published"
-- >   offValue "draft"
-- >   onValue "published"
-- >   checked  -- if initially on
toggleField :: Text -> FieldBuilder -> FormBuilder
toggleField name builder =
  tellField $ buildField name ToggleField builder

--------------------------------------------------------------------------------
-- Hidden Fields

-- | Add a hidden field with a name and value.
--
-- > hidden "csrf_token" csrfToken
-- > hidden "user_id" (display userId)
hidden :: Text -> Text -> FormBuilder
hidden = tellHidden

--------------------------------------------------------------------------------
-- Structure

-- | Create a form section with a title.
--
-- Fields added within the section are grouped together.
--
-- > section "Personal Information" do
-- >   textField "first_name" do
-- >     label "First Name"
-- >   textField "last_name" do
-- >     label "Last Name"
section :: Text -> FormBuilder -> FormBuilder
section title builder =
  let state = runFormBuilder builder
      -- Extract fields from elements (sections within sections become flattened)
      extractFields :: FormElement -> [Field]
      extractFields (FieldElement f) = [f]
      extractFields (SectionElement s) = secFields s
      sec =
        Section
          { secTitle = title,
            secFields = concatMap extractFields (fsElements state),
            secCondition = Nothing
          }
   in tellSection sec

-- | Conditionally render form content.
--
-- > conditional hasUpcomingDates
-- >   (selectField "date" dateOptions do label "Date")
-- >   (plain $ Lucid.p_ "No dates available")
conditional :: Bool -> FormBuilder -> FormBuilder -> FormBuilder
conditional cond trueBuilder falseBuilder =
  if cond then trueBuilder else falseBuilder

-- | Insert plain HTML (escape hatch).
--
-- Use this when you need custom HTML that doesn't fit the form builder model.
--
-- > plain $ Lucid.div_ [Lucid.class_ "my-4"] do
-- >   Lucid.p_ "Custom content here"
plain :: Lucid.Html () -> FormBuilder
plain html =
  tellField $
    Field
      { fName = "",
        fType = PlainHtmlField html,
        fConfig = defaultFieldConfig,
        fValidation = defaultValidation
      }

--------------------------------------------------------------------------------
-- Footer Items (Buttons and Inline Controls)

-- | Add a submit button to the form.
--
-- > submitButton "SAVE CHANGES"
submitButton :: Text -> FormBuilder
submitButton lbl = tellFooterItem (FooterSubmit lbl)

-- | Add a cancel button (renders as a link styled as button).
--
-- > cancelButton "/dashboard" "CANCEL"
cancelButton :: Text -> Text -> FormBuilder
cancelButton url lbl = tellFooterItem (FooterCancel url lbl)

-- | Add a toggle switch to the form footer (inline with buttons).
--
-- Use this for action modifiers like "publish/draft" that belong next to submit.
-- For toggles within form sections, use 'toggleField' instead.
--
-- > footerToggle "status" do
-- >   offLabel "DRAFT"
-- >   onLabel "PUBLISHED"
-- >   offValue "draft"
-- >   onValue "published"
-- >   checked  -- if initially on
footerToggle :: Text -> FieldBuilder -> FormBuilder
footerToggle name builder =
  tellFooterItem $ FooterToggle $ buildField name ToggleField builder

-- | Add a hint below the form footer.
--
-- Use this to provide context about the footer actions.
--
-- > footerHint "Saving as draft will not notify subscribers"
footerHint :: Text -> FormBuilder
footerHint = tellFooterHint
