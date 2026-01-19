{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import Component.Form.Builder
import Component.PageHeader (pageHeader)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
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

userForgotPasswordGetUrl :: Link.URI
userForgotPasswordGetUrl = Link.linkURI userLinks.forgotPasswordGet

--------------------------------------------------------------------------------

template :: Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template emailAddress redirectLink =
  let emailValue = maybe "" display emailAddress
      redirectLink' = fromMaybe "/" redirectLink
      hasError = isJust emailAddress
      postUrl = [i|/#{userLoginPostUrl redirectLink'}|]
   in Lucid.div_
        [class_ $ base [Tokens.fullWidth, "max-w-md", "mx-auto"]]
        do
          Lucid.div_ [class_ $ base [Tokens.bgWhite, "p-8"]] do
            pageHeader "LOGIN"

            -- Error alert (shown when login failed)
            when hasError alert

            -- Login form using form builder
            renderForm (loginFormConfig postUrl) (loginForm emailValue)

            -- Register section (outside form)
            registerSection

--------------------------------------------------------------------------------
-- Form Configuration

loginFormConfig :: Text -> FormConfig
loginFormConfig postUrl =
  defaultFormConfig
    { fcAction = postUrl,
      fcMethod = "post",
      -- Use "body" as target to enable HTMX submission
      -- The HX-Redirect header from the POST handler will handle the actual redirect
      fcHtmxTarget = Just "body"
    }

--------------------------------------------------------------------------------
-- Form Definition

loginForm :: Text -> FormBuilder
loginForm emailValue = do
  textField "email" $ do
    label "Email"
    placeholder "your@email.com"
    value emailValue
    required

  passwordField "password" $ do
    label "Password"
    placeholder "Your password"
    required

  -- Remember me + Forgot password row (custom HTML)
  plain rememberMeRow

  submitButton "LOGIN"

--------------------------------------------------------------------------------
-- Custom Components

rememberMeRow :: Lucid.Html ()
rememberMeRow =
  Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.textSm, "mb-4"]] do
    Lucid.div_ [class_ $ base ["flex", "items-center"]] do
      Lucid.input_ [Lucid.type_ "checkbox", Lucid.id_ "remember", Lucid.name_ "remember", Lucid.class_ "mr-2"]
      Lucid.label_ [Lucid.for_ "remember"] "Remember me"
    Lucid.a_
      [ Lucid.href_ [i|/#{userForgotPasswordGetUrl}|],
        hxGet_ [i|/#{userForgotPasswordGetUrl}|],
        hxSwap_ "innerHTML",
        hxTarget_ "body",
        hxPushUrl_ "true",
        class_ $ base ["text-blue-600", "hover:text-blue-800", "hover:underline"]
      ]
      "Forgot password?"

registerSection :: Lucid.Html ()
registerSection =
  Lucid.div_ [class_ $ base ["mt-8", "pt-6", "border-t", "border-gray-300", "text-center"]] do
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb4]] "Don't have an account yet?"
    Lucid.a_
      [ Lucid.href_ "#",
        hxGet_ [i|/#{userRegisterGetUrl}|],
        hxSwap_ "innerHTML",
        hxTarget_ "body",
        hxPushUrl_ "true",
        class_ $ base ["bg-green-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-green-700", "inline-block"]
      ]
      "CREATE ACCOUNT"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, "text-red-800", "rounded-lg", "bg-red-50"], Lucid.role_ "alert"]
    "Your email address or password is invalid."
