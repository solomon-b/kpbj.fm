{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Register.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import Component.PageHeader (pageHeader)
import Data.String.Interpolate (i)
import Data.Text
import Data.Text.Display (display)
import Design (base, class_, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userRegisterPostUrl :: Link.URI
userRegisterPostUrl = Link.linkURI userLinks.registerPost

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI (userLinks.loginGet Nothing Nothing)

privacyPolicyGetUrl :: Link.URI
privacyPolicyGetUrl = Link.linkURI apiLinks.privacyPolicyGet

termsOfServiceGetUrl :: Link.URI
termsOfServiceGetUrl = Link.linkURI apiLinks.termsOfServiceGet

--------------------------------------------------------------------------------

alpineState :: Text -> Text -> Text -> Text
alpineState displayNameValue fullNameValue emailValue =
  [i|{
  fields: {
    displayName: { value: `#{displayNameValue}`, isValid: true },
    fullName: { value: `#{fullNameValue}`, isValid: true },
    email: { value: `#{emailValue}`, isValid: true },
    password: { value: ``, isValid: true },
    confirmPassword: { value: ``, isValid: true }
  },
  termsAccepted: false,
  showErrors: false,
  get termsValid() { return this.termsAccepted; },

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate all fields
    this.fields.displayName.isValid = this.fields.displayName.value.trim() !== '';
    this.fields.fullName.isValid = this.fields.fullName.value.trim() !== '';
    this.fields.email.isValid = this.fields.email.value.trim() !== '';
    this.fields.password.isValid = this.fields.password.value.trim() !== '';
    this.fields.confirmPassword.isValid = this.fields.confirmPassword.value.trim() !== '' && this.fields.confirmPassword.value === this.fields.password.value;

    // Check if all fields are valid including terms
    const allFieldsValid = Object.values(this.fields).every(field => field.isValid) && this.termsValid;

    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

template :: Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template displayName fullName emailAddress _redirectLink = do
  let validationNotice = maybe "" (const alert) emailAddress
      displayNameValue = maybe "" display displayName
      fullNameValue = maybe "" display fullName
      emailValue = maybe "" display emailAddress
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto"], xData_ (alpineState displayNameValue fullNameValue emailValue)] do
    Lucid.div_ [class_ $ base [Tokens.bgWhite, "p-8"]] do
      pageHeader "CREATE ACCOUNT"
      Lucid.form_ [hxPost_ [i|/#{userRegisterPostUrl}|], Lucid.class_ "space-y-6"] do
        validationNotice

        Lucid.div_ [Lucid.class_ "space-y-4"] do
          fullNameField
          emailField

        Lucid.div_ [Lucid.class_ "space-y-4"] do
          displayNameField
          Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap4]; tablet ["grid-cols-2"] }] do
            passwordField
            confirmPasswordField
          passwordRequirements

        Lucid.div_ [Lucid.class_ "space-y-4"] do
          newsletterCheckbox
          termsCheckbox

        submitButton

      Lucid.div_ [class_ $ base ["mt-8", "pt-6", "border-t", "border-gray-300", "text-center"]] do
        Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb4]] "Already have an account?"
        Lucid.a_ [Lucid.href_ [i|/#{userLoginGetUrl}|], hxGet_ [i|/#{userLoginGetUrl}|], hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700", "inline-block"]] "LOGIN"

displayNameField :: Lucid.Html ()
displayNameField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "displayName", class_ $ base ["block", Tokens.fontBold, Tokens.mb2]]
      "Username *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "displayName",
        Lucid.id_ "displayName",
        class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.placeholder_ "Choose a unique username",
        Lucid.required_ "",
        xModel_ "fields.displayName.value",
        xBindClass_ "showErrors && !fields.displayName.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.textGray600, "mt-1"]] "This will be your public display name on the site"

fullNameField :: Lucid.Html ()
fullNameField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "fullName", class_ $ base ["block", Tokens.fontBold, Tokens.mb2]]
      "Full Name *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "fullName",
        Lucid.id_ "fullName",
        class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.placeholder_ "Your full name",
        Lucid.required_ "",
        xModel_ "fields.fullName.value",
        xBindClass_ "showErrors && !fields.fullName.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

emailField :: Lucid.Html ()
emailField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "email", class_ $ base ["block", Tokens.fontBold, Tokens.mb2]]
      "Email Address *"
    Lucid.input_
      [ Lucid.type_ "email",
        Lucid.name_ "email",
        Lucid.id_ "email",
        class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.placeholder_ "your@email.com",
        Lucid.required_ "",
        xModel_ "fields.email.value",
        xBindClass_ "showErrors && !fields.email.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.textGray600, "mt-1"]] "We'll use this for account notifications and important updates"

passwordField :: Lucid.Html ()
passwordField =
  Lucid.div_ [] do
    Lucid.label_
      [Lucid.for_ "password", class_ $ base ["block", Tokens.fontBold, Tokens.mb2]]
      "Password *"
    Lucid.input_
      [ Lucid.type_ "password",
        Lucid.name_ "password",
        Lucid.id_ "password",
        Lucid.placeholder_ "Create a secure password",
        class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.required_ "",
        xModel_ "fields.password.value",
        xBindClass_ "showErrors && !fields.password.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

confirmPasswordField :: Lucid.Html ()
confirmPasswordField =
  Lucid.div_ [] do
    Lucid.label_
      [Lucid.for_ "confirmPassword", class_ $ base ["block", Tokens.fontBold, Tokens.mb2]]
      "Confirm Password *"
    Lucid.input_
      [ Lucid.type_ "password",
        Lucid.name_ "confirmPassword",
        Lucid.id_ "confirmPassword",
        Lucid.placeholder_ "Confirm your password",
        class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.required_ "",
        xModel_ "fields.confirmPassword.value",
        xBindClass_ "showErrors && !fields.confirmPassword.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

passwordRequirements :: Lucid.Html ()
passwordRequirements =
  Lucid.div_ [class_ $ base [Tokens.bgGray100, "border", "border-gray-300", Tokens.p3, Tokens.textXs]] do
    Lucid.div_ [class_ $ base [Tokens.fontBold, "mb-1"]] "Password Requirements:"
    Lucid.div_ "• At least 8 characters long"
    Lucid.div_ "• Include uppercase and lowercase letters"
    Lucid.div_ "• Include at least one number"

newsletterCheckbox :: Lucid.Html ()
newsletterCheckbox =
  Lucid.div_ [class_ $ base ["flex", "items-start"]] do
    Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "newsletter", Lucid.name_ "newsletter", Lucid.class_ "mr-3 mt-1", Lucid.value_ "on"]
    Lucid.label_ [Lucid.for_ "newsletter", Lucid.class_ Tokens.textSm] do
      Lucid.span_ [Lucid.class_ Tokens.fontBold] "Subscribe to our newsletter"
      Lucid.div_ [Lucid.class_ Tokens.textGray600] "Get updates about shows, events, and station news"

termsCheckbox :: Lucid.Html ()
termsCheckbox =
  Lucid.div_ [class_ $ base ["flex", "items-start"], xBindClass_ "showErrors && !termsValid ? 'flex items-start p-2 border-2 border-red-500 bg-red-50' : 'flex items-start'"] do
    Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "terms", Lucid.name_ "terms", Lucid.class_ "mr-3 mt-1", Lucid.value_ "on", Lucid.required_ "", xModel_ "termsAccepted"]
    Lucid.label_ [Lucid.for_ "terms", Lucid.class_ Tokens.textSm] do
      Lucid.span_ [Lucid.class_ Tokens.fontBold] "I agree to the Terms of Service and Privacy Policy *"
      Lucid.div_ [Lucid.class_ Tokens.textGray600] do
        Lucid.span_ "By creating an account, you agree to our "
        Lucid.a_ [Lucid.href_ [i|/#{termsOfServiceGetUrl}|], hxGet_ [i|/#{termsOfServiceGetUrl}|], hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", class_ $ base ["text-blue-600", "hover:underline"]] "Terms of Service"
        Lucid.span_ " and "
        Lucid.a_ [Lucid.href_ [i|/#{privacyPolicyGetUrl}|], hxGet_ [i|/#{privacyPolicyGetUrl}|], hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", class_ $ base ["text-blue-600", "hover:underline"]] "Privacy Policy"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      class_ $ base [Tokens.fullWidth, "bg-green-600", Tokens.textWhite, Tokens.p3, Tokens.fontBold, "hover:bg-green-700", "transition-colors"],
      xOnClick_ "validateAndSubmit($event)"
    ]
    "CREATE MY ACCOUNT"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, "text-red-800", "rounded-lg", "bg-red-50"], Lucid.role_ "alert"]
    "Your email address or password is invalid."
