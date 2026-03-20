{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Invite.Token.Get.Templates.Page
  ( template,
    alreadyLoggedInTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (renderScheduleSummary)
import API.Links (apiLinks, inviteLinks, rootLink, userLinks)
import API.Types (InviteRoutes (..), Routes (..), UserRoutes (..))
import Data.Aeson (Value)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Template for the invite onboarding form page.
--
-- Renders a two-step form using Alpine.js for step visibility toggling.
-- Step 1 collects account information; step 2 collects show details.
-- The entire form submits as a single multipart POST.
template ::
  HostInvitation.Token ->  -- ^ Invitation token for form action URL
  HostInvitation.Model ->  -- ^ Invitation model with schedule data
  Lucid.Html ()
template token invitation =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8], xData_ "{ step: 1 }"] $
    Lucid.form_
      [ Lucid.action_ postUrl,
        Lucid.method_ "post",
        Lucid.enctype_ "multipart/form-data",
        hxPost_ postUrl,
        hxTarget_ "body",
        hxSwap_ "innerHTML",
        class_ $ base [Tokens.bgMain, "p-8"]
      ]
      $ do
        -- Step 1: Create Account
        step1 scheduleData

        -- Step 2: Set Up Your Show
        step2 scheduleData
  where
    postUrl :: Text
    postUrl =
      let link = Links.linkURI $ inviteLinks.onboardPost token
       in [i|/#{link}|]

    scheduleData :: Value
    scheduleData = invitation.hiScheduleData


--------------------------------------------------------------------------------
-- Step 1: Create Account

step1 :: Value -> Lucid.Html ()
step1 scheduleData =
  Lucid.div_ [xShow_ "step === 1", xCloak_] $ do
    -- Step indicator
    stepIndicator 1

    -- Welcome header
    Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] $
      "Welcome to KPBJ 95.9FM"
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] $
      "You've been invited to host a show. Let's get you set up."

    -- Schedule preview
    schedulePreview scheduleData

    -- Form fields
    formField "Full Name" $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "full_name",
          Lucid.placeholder_ "Your full name",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    formFieldWithHint "Email Address" "We'll use this for account notifications and important updates" $ do
      Lucid.input_
        [ Lucid.type_ "email",
          Lucid.name_ "email",
          Lucid.placeholder_ "your@email.com",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    formFieldWithHint "Host Name" "This is the name you will use as a show host. It will appear on your show page and episode listings." $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "display_name",
          Lucid.placeholder_ "Choose a host name",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    formField "Password" $ do
      Lucid.input_
        [ Lucid.type_ "password",
          Lucid.name_ "password",
          Lucid.placeholder_ "Create a secure password",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    formField "Confirm Password" $ do
      Lucid.input_
        [ Lucid.type_ "password",
          Lucid.name_ "confirm_password",
          Lucid.placeholder_ "Confirm your password",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    -- Password requirements
    passwordRequirements

    -- Newsletter checkbox
    checkboxField "newsletter" "Subscribe to our newsletter" $
      Just "Get updates about shows, events, and station news"

    -- Terms checkbox
    termsCheckbox

    -- Next button
    Lucid.div_ [class_ $ base [Tokens.mt8]] $
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "step = 2",
          class_ $ base [Tokens.buttonPrimary, Tokens.fullWidth]
        ]
        "Next: Set Up Your Show \x2192"

    -- Login link
    loginSection


--------------------------------------------------------------------------------
-- Step 2: Set Up Your Show

step2 :: Value -> Lucid.Html ()
step2 scheduleData =
  Lucid.div_ [xShow_ "step === 2", xCloak_] $ do
    -- Step indicator
    stepIndicator 2

    -- Header
    Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] $
      "Tell us about your show"
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] $
      "You can always update these details later from your dashboard."

    -- Show fields
    formField "Show Title" $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "show_title",
          Lucid.placeholder_ "Name your show",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    formField "Description" $ do
      Lucid.textarea_
        [ Lucid.name_ "show_description",
          Lucid.placeholder_ "What's your show about?",
          Lucid.rows_ "4",
          class_ $ base inputClasses
        ]
        ""

    formFieldWithHint "Tags" "Comma-separated, e.g. jazz, soul, funk" $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "show_tags",
          Lucid.placeholder_ "jazz, soul, funk",
          class_ $ base inputClasses
        ]

    formFieldWithHint "Show Logo" "Square image recommended. PNG or JPG." $ do
      Lucid.input_
        [ Lucid.type_ "file",
          Lucid.name_ "show_logo",
          Lucid.accept_ "image/*",
          class_ $ base inputClasses
        ]

    -- Schedule reminder
    schedulePreview scheduleData

    -- Back and Submit buttons
    Lucid.div_ [class_ $ base [Tokens.mt8, "flex", Tokens.gap4]] $ do
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "step = 1",
          class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px6, Tokens.py2, Tokens.fontBold, "border", Tokens.borderMuted, Tokens.hoverBg]
        ]
        "\x2190 Back"
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $ base [Tokens.buttonPrimary, "flex-1"]
        ]
        "Create Account & Show"


--------------------------------------------------------------------------------
-- Shared Components

-- | Step indicator showing progress through the two-step form.
stepIndicator :: Int -> Lucid.Html ()
stepIndicator currentStep =
  Lucid.div_ [class_ $ base ["flex", Tokens.gap4, Tokens.mb8, Tokens.textSm]] $ do
    -- Step 1
    Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2, step1Style]] $ do
      Lucid.span_ [class_ $ base stepBadgeClasses] stepOneLabel
      Lucid.span_ "Create Account"
    -- Separator
    Lucid.span_ [class_ $ base [Tokens.fgMuted]] "\x2014"
    -- Step 2
    Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2, step2Style]] $ do
      Lucid.span_ [class_ $ base stepBadgeClasses] "2"
      Lucid.span_ "Set Up Your Show"
  where
    step1Style = if currentStep >= 1 then Tokens.fgPrimary else Tokens.fgMuted
    step2Style = if currentStep >= 2 then Tokens.fgPrimary else Tokens.fgMuted

    stepOneLabel :: Lucid.Html ()
    stepOneLabel = if currentStep > 1 then "\x2713" else "1"

    stepBadgeClasses = [Tokens.fontBold]


-- | Read-only schedule preview card.
schedulePreview :: Value -> Lucid.Html ()
schedulePreview scheduleData =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, "border", Tokens.borderMuted, Tokens.p4, Tokens.mb6]] $ do
    Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, Tokens.fontBold, Tokens.mb2, "uppercase"]] $
      "Your Assigned Timeslot"
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgPrimary, Tokens.fontBold]] $
      Lucid.toHtml (renderScheduleSummary scheduleData)


-- | Standard form field wrapper with label.
formField :: Text -> Lucid.Html () -> Lucid.Html ()
formField labelText content =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
    Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] $
      Lucid.toHtml labelText
    content


-- | Form field wrapper with label and hint text.
formFieldWithHint :: Text -> Text -> Lucid.Html () -> Lucid.Html ()
formFieldWithHint labelText hintText content =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
    Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] $
      Lucid.toHtml labelText
    content
    Lucid.p_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "mt-1"]] $
      Lucid.toHtml hintText


-- | Common input classes.
inputClasses :: [Text]
inputClasses = [Tokens.fullWidth, Tokens.bgAlt, "border", Tokens.borderMuted, Tokens.p3, Tokens.textSm, "focus:outline-none", "focus:border-[var(--theme-info)]"]


-- | Password requirements info box.
passwordRequirements :: Lucid.Html ()
passwordRequirements =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, "border", Tokens.borderMuted, Tokens.p3, Tokens.textXs, Tokens.mb4]] $ do
    Lucid.div_ [class_ $ base [Tokens.fontBold, "mb-1"]] "Password Requirements:"
    Lucid.div_ "\x2022 At least 8 characters long"
    Lucid.div_ "\x2022 Include uppercase and lowercase letters"
    Lucid.div_ "\x2022 Include at least one number"


-- | Checkbox field with optional description.
checkboxField :: Text -> Text -> Maybe Text -> Lucid.Html ()
checkboxField fieldName labelText mDescription =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $
    Lucid.label_ [class_ $ base ["flex", "items-start", Tokens.gap2, "cursor-pointer"]] $ do
      Lucid.input_
        [ Lucid.type_ "checkbox",
          Lucid.name_ fieldName,
          class_ $ base ["mt-1", "shrink-0"]
        ]
      Lucid.div_ $ do
        Lucid.span_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml labelText
        case mDescription of
          Just desc ->
            Lucid.p_ [class_ $ base [Tokens.textXs, Tokens.fgMuted]] $ Lucid.toHtml desc
          Nothing -> pure ()


-- | Terms of Service checkbox with links.
termsCheckbox :: Lucid.Html ()
termsCheckbox =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $
    Lucid.label_ [class_ $ base ["flex", "items-start", Tokens.gap2, "cursor-pointer"]] $ do
      Lucid.input_
        [ Lucid.type_ "checkbox",
          Lucid.name_ "terms",
          Lucid.required_ "",
          class_ $ base ["mt-1", "shrink-0"]
        ]
      Lucid.div_ [class_ $ base [Tokens.textSm]] $ do
        Lucid.span_ "I agree to the "
        Lucid.a_
          [ Lucid.href_ termsUrl,
            hxGet_ termsUrl,
            hxSwap_ "innerHTML",
            hxTarget_ "body",
            hxPushUrl_ "true",
            class_ $ base [Tokens.linkText]
          ]
          "Terms of Service"
        Lucid.span_ " and "
        Lucid.a_
          [ Lucid.href_ privacyUrl,
            hxGet_ privacyUrl,
            hxSwap_ "innerHTML",
            hxTarget_ "body",
            hxPushUrl_ "true",
            class_ $ base [Tokens.linkText]
          ]
          "Privacy Policy"
  where
    termsUrl = rootLink apiLinks.termsOfServiceGet
    privacyUrl = rootLink apiLinks.privacyPolicyGet


-- | Login section at bottom of step 1.
loginSection :: Lucid.Html ()
loginSection =
  Lucid.div_ [class_ $ base [Tokens.mt8, "pt-6", "border-t", Tokens.borderMuted, "text-center"]] $ do
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]] "Already have an account?"
    Lucid.a_
      [ Lucid.href_ loginUrl,
        hxGet_ loginUrl,
        hxSwap_ "innerHTML",
        hxTarget_ "body",
        hxPushUrl_ "true",
        class_ $ base [Tokens.infoBg, Tokens.infoText, Tokens.px6, "py-3", Tokens.fontBold, Tokens.hoverBg, "inline-block"]
      ]
      "LOGIN"
  where
    loginUrl = rootLink $ userLinks.loginGet Nothing Nothing


--------------------------------------------------------------------------------
-- Already Logged In Template

-- | Template shown when a logged-in user visits an invitation link.
alreadyLoggedInTemplate :: Lucid.Html ()
alreadyLoggedInTemplate =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8]] $
    Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8", "text-center"]] $ do
      Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] $
        "You already have an account"
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb8]] $
        "Contact staff to be assigned to a show."
      Lucid.a_
        [ Lucid.href_ homeUrl,
          hxGet_ homeUrl,
          hxSwap_ "innerHTML",
          hxTarget_ "body",
          hxPushUrl_ "true",
          class_ $ base [Tokens.linkText]
        ]
        "\x2190 Back to kpbj.fm"
  where
    homeUrl = rootLink apiLinks.rootGet
