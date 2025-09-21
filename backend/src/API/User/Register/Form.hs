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
import Lucid (button_, class_, disabled_, div_, for_, form_, h3_, id_, input_, label_, name_, placeholder_, role_, script_, type_, value_)
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
  div_ do
    label_
      [for_ "displayName", class_ "block mb-2 text-sm font-medium text-gray-900"]
      "What should we call you?"
    input_
      [ type_ "displayName",
        name_ "displayName",
        id_ "displayName",
        class_ "border text-sm rounded-lg block w-full p-2.5",
        xModel_ "fields.displayName.value",
        xData_ "alpineHandler",
        xBindClass_ "fields.displayName.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
        xOnBlur_ "validateField('displayName')",
        xOnInput_ "validateField('displayName')"
      ]

fullNameField :: Lucid.Html ()
fullNameField =
  div_ do
    label_
      [for_ "fullName", class_ "block mb-2 text-sm font-medium text-gray-900"]
      "What is your full name?"
    input_
      [ type_ "fullName",
        name_ "fullName",
        id_ "fullName",
        class_ "border text-sm rounded-lg block w-full p-2.5",
        xModel_ "fields.fullName.value",
        xData_ "alpineHandler",
        xBindClass_ "fields.fullName.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
        xOnBlur_ "validateField('fullName')",
        xOnInput_ "validateField('fullName')"
      ]

emailField :: Lucid.Html ()
emailField =
  div_ do
    label_
      [for_ "email", class_ "block mb-2 text-sm font-medium text-gray-900"]
      "Your email"
    input_
      [ type_ "email",
        name_ "email",
        id_ "email",
        class_ "border text-sm rounded-lg block w-full p-2.5",
        placeholder_ "name@company.com",
        xModel_ "fields.email.value",
        xData_ "alpineHandler",
        xBindClass_ "fields.email.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
        xOnBlur_ "validateField('email')",
        xOnInput_ "validateField('email')"
      ]

passwordField :: Lucid.Html ()
passwordField =
  div_ [] do
    label_
      [for_ "password", class_ "block mb-2 text-sm font-medium text-gray-900"]
      "Your password"
    input_
      [ type_ "password",
        name_ "password",
        id_ "password",
        placeholder_ "••••••••",
        class_ "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5",
        xModel_ "fields.password.value",
        xData_ "alpineHandler",
        xBindClass_ "fields.password.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
        xOnBlur_ "validateField('password')",
        xOnInput_ "validateField('password')"
      ]

rememberMeToggle :: Lucid.Html ()
rememberMeToggle =
  div_ [class_ "flex justify-between"] do
    div_ [class_ "flex items-start"] do
      div_ [class_ "flex items-center h-5"] do
        input_ [id_ "remember", type_ "checkbox", value_ "", class_ "w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-green-300"]
      label_
        [for_ "remember", class_ "ms-2 text-sm font-medium text-gray-900"]
        "Remember me"

submitButton :: Lucid.Html ()
submitButton =
  button_
    [ type_ "submit",
      class_ "w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center",
      xData_ "alpineHandler",
      xBindDisabled_ "!allValid()",
      disabled_ "true"
    ]
    "Create your account"

alert :: Lucid.Html ()
alert =
  div_
    [class_ "p-4 mb-4 text-sm text-red-800 rounded-lg bg-red-50 dark:bg-gray-800 dark:text-red-400", role_ "alert"]
    "Your email address or password is invalid."

template :: Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template displayName fullName emailAddress _redirectLink =
  let validationNotice = maybe "" (const alert) emailAddress
      displayNameValue = maybe "" display displayName
      fullNameValue = maybe "" display fullName
      emailValue = maybe "" display emailAddress
   in div_
        [ class_ "relative p-4 w-full max-w-md max-h-full mx-auto",
          xData_ [i|{ fields: { displayName: { value: `#{displayNameValue}`, isValid: true }, fullName: { value: `#{fullNameValue}`, isValid: true }, email: { value: `#{emailValue}`, isValid: true }, password: { value: ``, isValid: true }}}|]
        ]
        do
          div_ [class_ "relative bg-white rounded-lg shadow"] do
            div_ [class_ "flex items-center justify-between p-4 md:p-5 border-b rounded-t"] do
              h3_
                [class_ "text-xl font-semibold text-gray-900"]
                "Sign in"
            div_ [class_ "p-4 md:p-5"] do
              form_ [hxPost_ [i|/#{userRegisterGetUrl}|], class_ "space-y-4"] do
                validationNotice
                displayNameField
                fullNameField
                emailField
                passwordField
                rememberMeToggle
                submitButton
              javascript

javascript :: Lucid.Html ()
javascript =
  script_
    [i|
  function validateField(field) {
     this.fields[field].isValid = this.fields[field].value.trim() !== '';
  };

  function allValid() {
    return Object.values(this.fields).every(field => field.isValid && field.value.trim() !== '');
  };

  /------------------------------------------------------------------------------/

  function alpineHandler() {
    return {
      validateField,
      allValid,
    };
  }
|]
