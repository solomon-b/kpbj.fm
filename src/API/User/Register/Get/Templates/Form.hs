{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.User.Register.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import Component.Form.Builder
import Component.PageHeader (pageHeader)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
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

template :: Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> Maybe Text -> Lucid.Html ()
template mDisplayName mFullName mEmailAddress _redirectLink =
  let displayNameValue = maybe "" display mDisplayName
      fullNameValue = maybe "" display mFullName
      emailValue = maybe "" display mEmailAddress
      hasError = isJust mEmailAddress
      postUrl = [i|/#{userRegisterPostUrl}|]
   in Lucid.div_
        [class_ $ base ["max-w-2xl", "mx-auto"]]
        do
          Lucid.div_ [class_ $ base [Tokens.bgWhite, "p-8"]] do
            pageHeader "CREATE ACCOUNT"

            -- Error alert (shown when registration failed)
            when hasError alert

            -- Registration form using form builder
            renderForm (registerFormConfig postUrl) (registerForm displayNameValue fullNameValue emailValue)

            -- Login section (outside form)
            loginSection

--------------------------------------------------------------------------------
-- Form Configuration

registerFormConfig :: Text -> FormConfig
registerFormConfig postUrl =
  defaultFormConfig
    { fcAction = postUrl,
      fcMethod = "post",
      fcHtmxTarget = Just "body"
    }

--------------------------------------------------------------------------------
-- Form Definition

registerForm :: Text -> Text -> Text -> FormBuilder
registerForm displayNameValue fullNameValue emailValue = do
  -- Personal info section
  textField "fullName" $ do
    label "Full Name"
    placeholder "Your full name"
    value fullNameValue
    required

  textField "email" $ do
    label "Email Address"
    placeholder "your@email.com"
    value emailValue
    hint "We'll use this for account notifications and important updates"
    required

  -- Account info section
  textField "displayName" $ do
    label "Username"
    placeholder "Choose a unique username"
    value displayNameValue
    hint "This will be your public display name on the site"
    required

  passwordField "password" $ do
    label "Password"
    placeholder "Create a secure password"
    required

  passwordField "confirmPassword" $ do
    label "Confirm Password"
    placeholder "Confirm your password"
    required

  -- Password requirements info
  plain passwordRequirements

  -- Newsletter subscription
  checkboxField "newsletter" $ do
    label "Subscribe to our newsletter"
    description $ Lucid.toHtml ("Get updates about shows, events, and station news" :: Text)

  -- Terms acceptance
  checkboxField "terms" $ do
    label "I agree to the Terms of Service and Privacy Policy"
    description termsDescription
    required

  submitButton "CREATE MY ACCOUNT"

--------------------------------------------------------------------------------
-- Custom Components

passwordRequirements :: Lucid.Html ()
passwordRequirements =
  Lucid.div_ [class_ $ base [Tokens.bgGray100, "border", "border-gray-300", Tokens.p3, Tokens.textXs, "mb-4"]] do
    Lucid.div_ [class_ $ base [Tokens.fontBold, "mb-1"]] "Password Requirements:"
    Lucid.div_ "• At least 8 characters long"
    Lucid.div_ "• Include uppercase and lowercase letters"
    Lucid.div_ "• Include at least one number"

termsDescription :: Lucid.Html ()
termsDescription = do
  Lucid.span_ "By creating an account, you agree to our "
  Lucid.a_
    [ Lucid.href_ [i|/#{termsOfServiceGetUrl}|],
      hxGet_ [i|/#{termsOfServiceGetUrl}|],
      hxSwap_ "innerHTML",
      hxTarget_ "body",
      hxPushUrl_ "true",
      class_ $ base ["text-blue-600", "hover:underline"]
    ]
    "Terms of Service"
  Lucid.span_ " and "
  Lucid.a_
    [ Lucid.href_ [i|/#{privacyPolicyGetUrl}|],
      hxGet_ [i|/#{privacyPolicyGetUrl}|],
      hxSwap_ "innerHTML",
      hxTarget_ "body",
      hxPushUrl_ "true",
      class_ $ base ["text-blue-600", "hover:underline"]
    ]
    "Privacy Policy"

loginSection :: Lucid.Html ()
loginSection =
  Lucid.div_ [class_ $ base ["mt-8", "pt-6", "border-t", "border-gray-300", "text-center"]] do
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb4]] "Already have an account?"
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxSwap_ "innerHTML",
        hxTarget_ "body",
        hxPushUrl_ "true",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700", "inline-block"]
      ]
      "LOGIN"

alert :: Lucid.Html ()
alert =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, "text-red-800", "rounded-lg", "bg-red-50"], Lucid.role_ "alert"]
    "Your email address or password is invalid."
