{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Register.Form where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userLoginPostLink, userRegisterGetLink)
import Data.String.Interpolate (i)
import Data.Text
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userLoginPostUrl :: Maybe Text -> Link.URI
userLoginPostUrl = Link.linkURI . userLoginPostLink

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI (userRegisterGetLink Nothing Nothing Nothing)

displayNameField :: Lucid.Html ()
displayNameField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "displayName", Lucid.class_ "block font-bold mb-2"]
      "Username *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "displayName",
        Lucid.id_ "displayName",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.placeholder_ "Choose a unique username",
        Lucid.required_ "",
        xModel_ "fields.displayName.value",
        xBindClass_ "showErrors && !fields.displayName.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "This will be your public display name on the site"

fullNameField :: Lucid.Html ()
fullNameField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "fullName", Lucid.class_ "block font-bold mb-2"]
      "Full Name *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "fullName",
        Lucid.id_ "fullName",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.placeholder_ "Your full name",
        Lucid.required_ "",
        xModel_ "fields.fullName.value",
        xBindClass_ "showErrors && !fields.fullName.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

emailField :: Lucid.Html ()
emailField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "email", Lucid.class_ "block font-bold mb-2"]
      "Email Address *"
    Lucid.input_
      [ Lucid.type_ "email",
        Lucid.name_ "email",
        Lucid.id_ "email",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.placeholder_ "your@email.com",
        Lucid.required_ "",
        xModel_ "fields.email.value",
        xBindClass_ "showErrors && !fields.email.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
    Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "We'll use this for account notifications and important updates"

passwordField :: Lucid.Html ()
passwordField =
  Lucid.div_ [] do
    Lucid.label_
      [Lucid.for_ "password", Lucid.class_ "block font-bold mb-2"]
      "Password *"
    Lucid.input_
      [ Lucid.type_ "password",
        Lucid.name_ "password",
        Lucid.id_ "password",
        Lucid.placeholder_ "Create a secure password",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.required_ "",
        xModel_ "fields.password.value",
        xBindClass_ "showErrors && !fields.password.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

confirmPasswordField :: Lucid.Html ()
confirmPasswordField =
  Lucid.div_ [] do
    Lucid.label_
      [Lucid.for_ "confirmPassword", Lucid.class_ "block font-bold mb-2"]
      "Confirm Password *"
    Lucid.input_
      [ Lucid.type_ "password",
        Lucid.name_ "confirmPassword",
        Lucid.id_ "confirmPassword",
        Lucid.placeholder_ "Confirm your password",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.required_ "",
        xModel_ "fields.confirmPassword.value",
        xBindClass_ "showErrors && !fields.confirmPassword.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

passwordRequirements :: Lucid.Html ()
passwordRequirements =
  Lucid.div_ [Lucid.class_ "bg-gray-100 border border-gray-300 p-3 text-xs"] do
    Lucid.div_ [Lucid.class_ "font-bold mb-1"] "Password Requirements:"
    Lucid.div_ "• At least 8 characters long"
    Lucid.div_ "• Include uppercase and lowercase letters"
    Lucid.div_ "• Include at least one number"

newsletterCheckbox :: Lucid.Html ()
newsletterCheckbox =
  Lucid.div_ [Lucid.class_ "flex items-start"] do
    Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "newsletter", Lucid.name_ "newsletter", Lucid.class_ "mr-3 mt-1", Lucid.value_ "on"]
    Lucid.label_ [Lucid.for_ "newsletter", Lucid.class_ "text-sm"] do
      Lucid.span_ [Lucid.class_ "font-bold"] "Subscribe to our newsletter"
      Lucid.div_ [Lucid.class_ "text-gray-600"] "Get updates about shows, events, and station news"

termsCheckbox :: Lucid.Html ()
termsCheckbox =
  Lucid.div_ [Lucid.class_ "flex items-start", xBindClass_ "showErrors && !termsValid ? 'flex items-start p-2 border-2 border-red-500 bg-red-50' : 'flex items-start'"] do
    Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "terms", Lucid.name_ "terms", Lucid.class_ "mr-3 mt-1", Lucid.value_ "on", Lucid.required_ "", xModel_ "termsAccepted"]
    Lucid.label_ [Lucid.for_ "terms", Lucid.class_ "text-sm"] do
      Lucid.span_ [Lucid.class_ "font-bold"] "I agree to the Terms of Service and Privacy Policy *"
      Lucid.div_ [Lucid.class_ "text-gray-600"] do
        Lucid.span_ "By creating an account, you agree to our "
        Lucid.a_ [Lucid.href_ "#", hxGet_ "/terms-of-service", hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ "text-blue-600 hover:underline"] "Terms of Service"
        Lucid.span_ " and "
        Lucid.a_ [Lucid.href_ "#", hxGet_ "/privacy-policy", hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ "text-blue-600 hover:underline"] "Privacy Policy"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "w-full bg-green-600 text-white p-3 font-bold hover:bg-green-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "CREATE MY ACCOUNT"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [Lucid.class_ "p-4 mb-4 text-sm text-red-800 rounded-lg bg-red-50 dark:bg-gray-800 dark:text-red-400", Lucid.role_ "alert"]
    "Your email address or password is invalid."

template :: Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template displayName fullName emailAddress _redirectLink =
  let validationNotice = maybe "" (const alert) emailAddress
      displayNameValue = maybe "" display displayName
      fullNameValue = maybe "" display fullName
      emailValue = maybe "" display emailAddress
   in Lucid.div_
        [ Lucid.class_ "max-w-2xl mx-auto",
          xData_ [i|{ 
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
        ]
        do
          Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8"] do
            Lucid.div_ [Lucid.class_ "text-center mb-8"] do
              Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-2"] "📝 CREATE ACCOUNT"
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Join the KPBJ community and unlock exclusive features"

            Lucid.form_ [hxPost_ [i|/#{userRegisterGetUrl}|], Lucid.class_ "space-y-6"] do
              validationNotice

              Lucid.div_ [Lucid.class_ "space-y-4"] do
                Lucid.h3_ [Lucid.class_ "font-bold text-lg border-b border-gray-300 pb-2"] "Personal Information"
                fullNameField
                emailField

              Lucid.div_ [Lucid.class_ "space-y-4"] do
                Lucid.h3_ [Lucid.class_ "font-bold text-lg border-b border-gray-300 pb-2"] "Account Setup"
                displayNameField
                Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-4"] do
                  passwordField
                  confirmPasswordField
                passwordRequirements

              Lucid.div_ [Lucid.class_ "space-y-4"] do
                newsletterCheckbox
                termsCheckbox

              submitButton

            Lucid.div_ [Lucid.class_ "mt-8 pt-6 border-t border-gray-300 text-center"] do
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-4"] "Already have an account?"
              Lucid.a_ [Lucid.href_ "/user/login", hxGet_ "/user/login", hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"] "LOGIN"

          Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6 mt-6"] do
            Lucid.h3_ [Lucid.class_ "font-bold mb-4"] "🎯 WHAT YOU GET WITH AN ACCOUNT"
            Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-4 text-sm"] do
              Lucid.div_ [Lucid.class_ "space-y-2"] do
                Lucid.div_ [Lucid.class_ "font-bold"] "Community Features:"
                Lucid.div_ "• Comment on blog posts"
                Lucid.div_ "• Submit event listings"
                Lucid.div_ "• Connect with other listeners"
                Lucid.div_ "• Join discussions"
              Lucid.div_ [Lucid.class_ "space-y-2"] do
                Lucid.div_ [Lucid.class_ "font-bold"] "Host Opportunities:"
                Lucid.div_ "• Apply to host your own show"
                Lucid.div_ "• Access host dashboard"
                Lucid.div_ "• Upload episode content"
                Lucid.div_ "• Manage show information"

            Lucid.div_ [Lucid.class_ "mt-4 p-3 bg-blue-50 border border-blue-400 text-sm"] do
              Lucid.div_ [Lucid.class_ "font-bold text-blue-800 mb-1"] "🎙️ Interested in Hosting?"
              Lucid.div_ [Lucid.class_ "text-blue-700"] "Create your account first, then contact our staff about hosting opportunities. We're always looking for passionate community members to join our DJ team!"

