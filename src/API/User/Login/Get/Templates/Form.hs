{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userLoginPostUrl :: Text -> Link.URI
userLoginPostUrl = Link.linkURI . userLinks.loginPost . Just

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI (userLinks.registerGet Nothing Nothing Nothing)

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
        [ Lucid.class_ $ cls [Tokens.fullWidth, "max-w-md", "mx-auto"],
          xData_ (alpineState emailValue)
        ]
        do
          Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, "p-8"]] do
            Lucid.div_ [Lucid.class_ $ cls ["text-center", Tokens.mb8]] do
              Lucid.h3_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "LOGIN"
              Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600]] "Access your KPBJ community account"
            Lucid.form_ [Lucid.class_ "space-y-6", hxPost_ [i|/#{userLoginPostUrl redirectLink'}|]] do
              validationNotice
              emailField
              passwordField
              lostPasswordField
              submitButton
            Lucid.div_ [Lucid.class_ $ cls ["mt-8", "pt-6", "border-t", "border-gray-300", "text-center"]] do
              Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600, Tokens.mb4]] "Don't have an account yet?"
              Lucid.a_
                [Lucid.href_ "#", hxGet_ [i|/#{userRegisterGetUrl}|], hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ $ cls ["bg-green-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-green-700", "inline-block"]]
                "CREATE ACCOUNT"

          Lucid.div_ [Lucid.class_ $ cls [Tokens.bgGray100, Tokens.border2, "border-gray-400", Tokens.p6, "mt-6"]] do
            Lucid.h3_ [Lucid.class_ $ cls [Tokens.fontBold, "mb-3"]] "JOIN OUR COMMUNITY"
            Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, "space-y-2", Tokens.textGray700]] do
              Lucid.div_ "• Comment on blog posts and events"
              Lucid.div_ "• Apply to host your own show"
              Lucid.div_ "• Submit event listings"
              Lucid.div_ "• Access exclusive content"
              Lucid.div_ "• Connect with other listeners"

emailField :: Lucid.Html ()
emailField =
  Lucid.div_ do
    Lucid.label_
      [Lucid.for_ "email", Lucid.class_ $ cls ["block", Tokens.fontBold, Tokens.mb2]]
      "Email or Username *"
    Lucid.input_
      [ Lucid.type_ "email",
        Lucid.name_ "email",
        Lucid.id_ "email",
        Lucid.class_ $ cls [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        Lucid.placeholder_ "your@email.com or username",
        xModel_ "fields.email.value",
        xBindClass_ "showErrors && !fields.email.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

passwordField :: Lucid.Html ()
passwordField =
  Lucid.div_ [] do
    Lucid.label_
      [Lucid.for_ "password", Lucid.class_ $ cls ["block", Tokens.fontBold, Tokens.mb2]]
      "Password *"
    Lucid.input_
      [ Lucid.type_ "password",
        Lucid.name_ "password",
        Lucid.id_ "password",
        Lucid.placeholder_ "Your password",
        Lucid.class_ $ cls [Tokens.fullWidth, Tokens.p3, Tokens.border2, "border-gray-400", "font-mono", "focus:border-blue-600"],
        xModel_ "fields.password.value",
        xBindClass_ "showErrors && !fields.password.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

lostPasswordField :: Lucid.Html ()
lostPasswordField =
  Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", "justify-between", Tokens.textSm]] do
    Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center"]] do
      Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "remember", Lucid.class_ "mr-2"]
      Lucid.label_ [Lucid.for_ "remember"] "Remember me"
    Lucid.a_ [Lucid.href_ "#", hxGet_ "/forgot-password", hxSwap_ "innerHTML", hxTarget_ "body", hxPushUrl_ "true", Lucid.class_ $ cls ["text-blue-600", "hover:text-blue-800", "hover:underline"]] "Forgot password?"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ $ cls [Tokens.fullWidth, "bg-blue-600", Tokens.textWhite, Tokens.p3, Tokens.fontBold, "hover:bg-blue-700", "transition-colors"],
      xOnClick_ "validateAndSubmit($event)"
    ]
    "LOGIN"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [Lucid.class_ $ cls [Tokens.p4, Tokens.mb4, Tokens.textSm, "text-red-800", "rounded-lg", "bg-red-50"], Lucid.role_ "alert"]
    "Your email address or password is invalid."
