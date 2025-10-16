{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, showGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnClick_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

alpineState :: Shows.Model -> Text
alpineState Shows.Model {title, description, genre, status, frequency, durationMinutes} =
  [i|{
  fields: {
    title: { value: `#{title}`, isValid: true },
    description: { value: `#{description}`, isValid: true },
    genre: { value: `#{maybe "" display genre}`, isValid: true },
    status: { value: `#{status}`, isValid: true },
    frequency: { value: `#{frequency}`, isValid: true },
    duration: { value: `#{maybe "" display durationMinutes}`, isValid: true }
  },
  showErrors: false,

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate all required fields
    this.fields.title.isValid = this.fields.title.value.trim() !== '';
    this.fields.description.isValid = this.fields.description.value.trim() !== '';
    this.fields.genre.isValid = this.fields.genre.value.trim() !== '';
    this.fields.status.isValid = this.fields.status.value.trim() !== '';
    this.fields.frequency.isValid = this.fields.frequency.value.trim() !== '';
    this.fields.duration.isValid = this.fields.duration.value.trim() !== '';

    // Check if all fields are valid
    const allFieldsValid = Object.values(this.fields).every(field => field.isValid);

    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

-- | Show edit template
template :: Shows.Model -> UserMetadata.Model -> Bool -> Lucid.Html ()
template showModel userMeta isStaff = do
  let showSlug = showModel.slug
      showBackUrl = showGetUrl showSlug

  Lucid.div_ [Lucid.class_ "max-w-2xl mx-auto", xData_ (alpineState showModel)] $ do
    header userMeta showModel showBackUrl

    -- Edit Show Form
    Lucid.form_ [Lucid.action_ [i|/shows/#{showSlug}/edit|], Lucid.method_ "post", Lucid.enctype_ "multipart/form-data", Lucid.class_ "space-y-8 w-full"] $ do
      -- Basic Information
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "BASIC INFORMATION"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          titleField showModel
          descriptionField showModel
          genreField showModel

      -- Schedule & Settings (Staff+ Only)
      if isStaff
        then Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "SCHEDULE & SETTINGS"
          Lucid.div_ [Lucid.class_ "space-y-6"] $ do
            statusField showModel
            frequencyField showModel
            durationField showModel
        else do
          -- Hidden fields to preserve existing values for non-staff
          Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ (case showModel.status of Shows.Active -> "active"; Shows.Inactive -> "inactive")]
          Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "frequency", Lucid.value_ (case showModel.frequency of Shows.Weekly -> "weekly"; Shows.Biweekly -> "biweekly"; Shows.Monthly -> "monthly"; Shows.Occasional -> "occasional"; Shows.OneTime -> "one-time")]
          Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "duration_minutes", Lucid.value_ (maybe "" (Text.pack . show) showModel.durationMinutes)]

      -- Artwork & Branding
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "ARTWORK & BRANDING"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          logoField showModel
          bannerField showModel

      -- Form Actions
      Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
          submitButton
          cancelButton showBackUrl

selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty

header :: UserMetadata.Model -> Shows.Model -> Links.URI -> Lucid.Html ()
header userMeta showModel showBackUrl = do
  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT SHOW"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBackUrl}|],
            hxGet_ [i|/#{showBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "DASHBOARD"

titleField :: Shows.Model -> Lucid.Html ()
titleField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        Lucid.required_ "true",
        Lucid.value_ showModel.title,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. Industrial Depths",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

descriptionField :: Shows.Model -> Lucid.Html ()
descriptionField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Description *"
    Lucid.textarea_
      [ Lucid.name_ "description",
        Lucid.required_ "true",
        Lucid.rows_ "6",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
        Lucid.placeholder_ "Describe your show. What kind of music do you play? What's your show's vibe?",
        xModel_ "fields.description.value",
        xBindClass_ "showErrors && !fields.description.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
      (Lucid.toHtml showModel.description)

genreField :: Shows.Model -> Lucid.Html ()
genreField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Genre"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "genre",
        Lucid.value_ (maybe "" display showModel.genre),
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. Techno, Ambient, Experimental, Hip-Hop",
        xModel_ "fields.genre.value",
        xBindClass_ "showErrors && !fields.genre.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Primary genre or style of music"

statusField :: Shows.Model -> Lucid.Html ()
statusField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show Status *"
    Lucid.select_
      [ Lucid.name_ "status",
        Lucid.required_ "true",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        xModel_ "fields.status.value",
        xBindClass_ "showErrors && !fields.status.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
      $ do
        Lucid.option_ [Lucid.value_ "active", selectedIf (showModel.status == Shows.Active)] "Active"
        Lucid.option_ [Lucid.value_ "inactive", selectedIf (showModel.status == Shows.Inactive)] "Inactive"
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Active shows appear on the shows page"

frequencyField :: Shows.Model -> Lucid.Html ()
frequencyField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Broadcast Frequency *"
    Lucid.select_
      [ Lucid.name_ "frequency",
        Lucid.required_ "true",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        xModel_ "fields.frequency.value",
        xBindClass_ "showErrors && !fields.frequency.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
      $ do
        Lucid.option_ [Lucid.value_ "weekly", selectedIf (showModel.frequency == Shows.Weekly)] "Weekly"
        Lucid.option_ [Lucid.value_ "biweekly", selectedIf (showModel.frequency == Shows.Biweekly)] "Biweekly"
        Lucid.option_ [Lucid.value_ "monthly", selectedIf (showModel.frequency == Shows.Monthly)] "Monthly"
        Lucid.option_ [Lucid.value_ "occasional", selectedIf (showModel.frequency == Shows.Occasional)] "Occasional"
        Lucid.option_ [Lucid.value_ "one-time", selectedIf (showModel.frequency == Shows.OneTime)] "One-time"

durationField :: Shows.Model -> Lucid.Html ()
durationField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Typical Duration (minutes)"
    Lucid.input_
      [ Lucid.type_ "number",
        Lucid.name_ "duration_minutes",
        Lucid.value_ (maybe "" (Text.pack . show) showModel.durationMinutes),
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. 120",
        Lucid.min_ "1",
        Lucid.step_ "1",
        xModel_ "fields.duration.value",
        xBindClass_ "showErrors && !fields.duration.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "How long is a typical episode?"

logoField :: Shows.Model -> Lucid.Html ()
logoField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Logo Image (Optional)"
    case showModel.logoUrl of
      Just currentLogo -> do
        Lucid.div_ [Lucid.class_ "mb-4 p-3 bg-gray-50 border border-gray-300"] $ do
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "Current logo:"
          Lucid.img_ [Lucid.src_ [i|/media/#{currentLogo}|], Lucid.alt_ "Show logo", Lucid.class_ "max-w-xs border border-gray-300"]
      Nothing ->
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mb-4 italic"] "No logo currently set"
    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-6 text-center"] $ do
      Lucid.input_
        [ Lucid.type_ "file",
          Lucid.name_ "logo_file",
          Lucid.accept_ "image/jpeg,image/png,image/webp,image/gif",
          Lucid.class_ "hidden",
          Lucid.id_ "logo-file"
        ]
      Lucid.label_ [Lucid.for_ "logo-file", Lucid.class_ "cursor-pointer"] $ do
        Lucid.div_ [Lucid.class_ "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block"] $
          "🖼️ CHOOSE LOGO IMAGE"
        Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "JPG, PNG, WebP, GIF accepted • Max 10MB • Recommended: 300x300px"

bannerField :: Shows.Model -> Lucid.Html ()
bannerField showModel =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Banner Image (Optional)"
    case showModel.bannerUrl of
      Just currentBanner -> do
        Lucid.div_ [Lucid.class_ "mb-4 p-3 bg-gray-50 border border-gray-300"] $ do
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "Current banner:"
          Lucid.img_ [Lucid.src_ [i|/media/#{currentBanner}|], Lucid.alt_ "Show banner", Lucid.class_ "max-w-full border border-gray-300"]
      Nothing ->
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mb-4 italic"] "No banner currently set"
    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-6 text-center"] $ do
      Lucid.input_
        [ Lucid.type_ "file",
          Lucid.name_ "banner_file",
          Lucid.accept_ "image/jpeg,image/png,image/webp,image/gif",
          Lucid.class_ "hidden",
          Lucid.id_ "banner-file"
        ]
      Lucid.label_ [Lucid.for_ "banner-file", Lucid.class_ "cursor-pointer"] $ do
        Lucid.div_ [Lucid.class_ "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block"] $
          "🖼️ CHOOSE BANNER IMAGE"
        Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "JPG, PNG, WebP, GIF accepted • Max 10MB • Recommended: 1200x300px"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "UPDATE SHOW"

cancelButton :: Links.URI -> Lucid.Html ()
cancelButton showBackUrl =
  Lucid.a_
    [ Lucid.href_ [i|/#{showBackUrl}|],
      hxGet_ [i|/#{showBackUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
    ]
    "CANCEL"
