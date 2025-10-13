{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Get.Templates.Form where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userLoginPostLink, userRegisterGetLink)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userLoginPostUrl :: Text -> Link.URI
userLoginPostUrl = Link.linkURI . userLoginPostLink . Just

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI (userRegisterGetLink Nothing Nothing Nothing)

--------------------------------------------------------------------------------

alpineState :: Text -> Text
alpineState emailValue =
  [i|{
  fields: {
    email: { value: `#{emailValue}`, isValid: true },
    password: { value: ``, isValid: true }
  },
  showErrors: false,

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate all fields
    this.fields.email.isValid = this.fields.email.value.trim() !== '';
    this.fields.password.isValid = this.fields.password.value.trim() !== '';

    // Check if all fields are valid including terms
    const allFieldsValid = Object.values(this.fields).every(field => field.isValid);

    // Validate all fields
    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

template :: Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template emailAddress redirectLink =
  let validationNotice = maybe "" (const alert) emailAddress
      emailValue = maybe "" display emailAddress
      redirectLink' = fromMaybe "/" redirectLink
   in Lucid.div_
        [ Lucid.class_ "w-full max-w-md mx-auto",
          xData_ (alpineState emailValue)
        ]
        do
          Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8"] do
            Lucid.div_ [Lucid.class_ "text-center mb-8"] do
              Lucid.h3_ [Lucid.class_ "text-2xl font-bold mb-2"] "🔐 LOGIN"
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Access your KPBJ community account"
            Lucid.form_ [Lucid.class_ "space-y-6", hxPost_ [i|/#{userLoginPostUrl redirectLink'}|]] do
              validationNotice
              emailField
              passwordField
              lostPasswordField
              submitButton
            Lucid.div_ [Lucid.class_ "mt-8 pt-6 border-t border-gray-300 text-center"] do
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-4"] "Don't have an account yet?"
              Lucid.a_
                [Lucid.href_ "#", hxGet_ [i|/#{userRegisterGetUrl}|], hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700 inline-block"]
                "CREATE ACCOUNT"

          Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6 mt-6"] do
            Lucid.h3_ [Lucid.class_ "font-bold mb-3"] "🎙️ JOIN OUR COMMUNITY"
            Lucid.div_ [Lucid.class_ "text-sm space-y-2 text-gray-700"] do
              Lucid.div_ "• Comment on blog posts and events"
              Lucid.div_ "• Apply to host your own show"
              Lucid.div_ "• Submit event listings"
              Lucid.div_ "• Access exclusive content"
              Lucid.div_ "• Connect with other listeners"

emailField :: Lucid.Html ()
emailField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "email", Lucid.class_ "block font-bold mb-2"]
      "Email or Username *"
    Lucid.input_
      [ Lucid.type_ "email",
        Lucid.name_ "email",
        Lucid.id_ "email",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        Lucid.placeholder_ "your@email.com or username",
        xModel_ "fields.email.value",
        xBindClass_ "showErrors && !fields.email.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

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
        Lucid.placeholder_ "Your password",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
        xModel_ "fields.password.value",
        xBindClass_ "showErrors && !fields.password.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

lostPasswordField :: Lucid.Html ()
lostPasswordField =
  Lucid.div_ [Lucid.class_ "flex items-center justify-between text-sm"] do
    Lucid.div_ [Lucid.class_ "flex items-center"] do
      Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "remember", Lucid.class_ "mr-2"]
      Lucid.label_ [Lucid.for_ "remember"] "Remember me"
    Lucid.a_ [Lucid.href_ "#", hxGet_ "/forgot-password", hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ "text-blue-600 hover:text-blue-800 hover:underline"] "Forgot password?"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "w-full bg-blue-600 text-white p-3 font-bold hover:bg-blue-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "LOGIN"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [Lucid.class_ "p-4 mb-4 text-sm text-red-800 rounded-lg bg-red-50 dark:bg-gray-800 dark:text-red-400", Lucid.role_ "alert"]
    "Your email address or password is invalid."
