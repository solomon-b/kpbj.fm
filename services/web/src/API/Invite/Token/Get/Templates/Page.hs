{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Invite.Token.Get.Templates.Page
  ( template,
    alreadyLoggedInTemplate,
    staffPreviewTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (formatDateTime, formatStartDate, renderScheduleSummary, renderStatusBadge)
import API.Links (apiLinks, dashboardInvitationsLinks, inviteLinks, rootLink, userLinks)
import API.Types (DashboardInvitationsRoutes (..), InviteRoutes (..), Routes (..), UserRoutes (..))
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.Form.Hints (markdownHint)
import Data.Aeson (Value)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (Day)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.EmailAddress (EmailAddress)
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
  -- | Invitation token for form action URL
  HostInvitation.Token ->
  -- | Invitation model with schedule data
  HostInvitation.Model ->
  Lucid.Html ()
template token invitation =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8], xData_ "{ step: 1 }"]
    $ Lucid.form_
      [ Lucid.action_ postUrl,
        Lucid.method_ "post",
        Lucid.enctype_ "multipart/form-data",
        hxPost_ postUrl,
        hxTarget_ "body",
        hxSwap_ "innerHTML",
        class_ $ base (Card.classes Card.Auth)
      ]
    $ do
      -- Step 1: Create Account
      step1 recipientEmail scheduleData startsOn

      -- Step 2: Set Up Your Show
      step2 scheduleData startsOn
  where
    postUrl :: Text
    postUrl =
      let link = Links.linkURI $ inviteLinks.onboardPost token
       in [i|/#{link}|]

    scheduleData :: Value
    scheduleData = invitation.hiScheduleData

    startsOn :: Day
    startsOn = invitation.hiStartsOn

    recipientEmail :: EmailAddress
    recipientEmail = invitation.hiRecipientEmail

--------------------------------------------------------------------------------
-- Step 1: Create Account

step1 :: EmailAddress -> Value -> Day -> Lucid.Html ()
step1 recipientEmail scheduleData startsOn =
  Lucid.div_ [Lucid.id_ "step1-fields", xShow_ "step === 1", xCloak_] $ do
    -- Step indicator
    stepIndicator 1

    -- Welcome header
    Lucid.h1_
      [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]]
      "Welcome to KPBJ 95.9FM"
    Lucid.p_
      [class_ $ base [Tokens.fgMuted, Tokens.mb6]]
      "You've been invited to host a show. Let's get you set up."

    -- Schedule preview
    schedulePreview scheduleData startsOn

    -- Form fields
    formField "Full Name" $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "full_name",
          Lucid.placeholder_ "Your full name",
          Lucid.required_ "",
          class_ $ base inputClasses
        ]

    -- Recipient email is fixed to the invitation. Rendered read-only and
    -- re-validated server-side. If it's wrong, the invitee must contact
    -- staff to update + resend the invitation.
    formFieldWithHint "Email Address" "This is the email this invitation was sent to. Contact staff if it's incorrect." $ do
      Lucid.input_
        [ Lucid.type_ "email",
          Lucid.name_ "email",
          Lucid.value_ (display recipientEmail),
          Lucid.readonly_ "",
          Lucid.required_ "",
          class_ $ base (inputClasses <> ["cursor-not-allowed", "opacity-70"])
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
          Lucid.minlength_ "8",
          class_ $ base inputClasses
        ]

    formField "Confirm Password" $ do
      Lucid.input_
        [ Lucid.type_ "password",
          Lucid.name_ "confirm_password",
          Lucid.placeholder_ "Confirm your password",
          Lucid.required_ "",
          Lucid.minlength_ "8",
          class_ $ base inputClasses
        ]

    -- Password requirements
    passwordRequirements

    -- Newsletter checkbox
    checkboxField "newsletter" "Subscribe to our newsletter" $
      Just "Get updates about shows, events, and station news"

    -- Terms checkbox
    termsCheckbox

    -- Next button. Step transition is gated on step-1 fields passing
    -- HTML5 constraint validation; reportValidity surfaces the first
    -- invalid field's native browser tooltip.
    Lucid.div_ [class_ $ base [Tokens.mt8]] $
      Button.render
        ( Button.defaults
            & Button.accent
            & Button.large
            & Button.square
            & Button.fullWidth
            & Button.with [xOnClick_ "if (Array.from(document.querySelectorAll('#step1-fields input, #step1-fields select, #step1-fields textarea')).every(el => el.reportValidity())) { step = 2 }"]
        )
        "Next: Set Up Your Show \x2192"

    -- Login link
    loginSection

--------------------------------------------------------------------------------
-- Step 2: Set Up Your Show

step2 :: Value -> Day -> Lucid.Html ()
step2 scheduleData startsOn =
  Lucid.div_ [xShow_ "step === 2", xCloak_] $ do
    -- Step indicator
    stepIndicator 2

    -- Header
    Lucid.h1_
      [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]]
      "Tell us about your show"
    Lucid.p_
      [class_ $ base [Tokens.fgMuted, Tokens.mb6]]
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

    formFieldWithHint "Description" markdownHint $ do
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
    schedulePreview scheduleData startsOn

    -- Back and Submit buttons
    Lucid.div_ [class_ $ base [Tokens.mt8, "flex", Tokens.gap4]] $ do
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "step = 1",
          class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px6, Tokens.py2, Tokens.fontBold, "border", Tokens.borderMuted, Tokens.hoverBg]
        ]
        "\x2190 Back"
      Button.render
        ( Button.defaults
            & Button.accent
            & Button.large
            & Button.square
            & Button.submit
            & Button.with [class_ $ base ["flex-1"]]
        )
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
schedulePreview :: Value -> Day -> Lucid.Html ()
schedulePreview scheduleData startsOn =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, "border", Tokens.borderMuted, Tokens.p4, Tokens.mb6]] $ do
    Lucid.div_
      [class_ $ base [Tokens.textXs, Tokens.fgMuted, Tokens.fontBold, Tokens.mb2, "uppercase"]]
      "Your Assigned Timeslot"
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgPrimary, Tokens.fontBold]] $
      Lucid.toHtml (renderScheduleSummary scheduleData)
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
      Lucid.toHtml ("Starts " <> formatStartDate startsOn)

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
    Button.render
      ( Button.defaults
          & Button.muted
          & Button.large
          & Button.square
          & Button.pageLink loginUrl
          & Button.with
            [ hxGet_ loginUrl,
              hxSwap_ "innerHTML",
              hxTarget_ "body",
              hxPushUrl_ "true",
              class_ $ base ["inline-block"]
            ]
      )
      "LOGIN"
  where
    loginUrl = rootLink $ userLinks.loginGet Nothing Nothing

--------------------------------------------------------------------------------
-- Already Logged In Template

-- | Template shown when a logged-in user who is not staff visits a valid
-- invitation link.
--
-- The claim creates a new account, so the user must log out first. The page
-- says that the invitation is not claimed, so that a host does not read it
-- as a successful claim.
alreadyLoggedInTemplate ::
  -- | Recipient email of the invitation
  EmailAddress ->
  Lucid.Html ()
alreadyLoggedInTemplate recipientEmail =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8]] $
    Card.card Card.Auth ["text-center"] $ do
      Lucid.h1_
        [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]]
        "This invitation is not claimed"
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb4]] $ do
        "This invitation is for "
        Lucid.span_ [class_ $ base [Tokens.fgPrimary, Tokens.fontBold]] $
          Lucid.toHtml (display recipientEmail)
        ". You are logged in, so you cannot claim it."
      Lucid.p_
        [class_ $ base [Tokens.fgMuted, Tokens.mb8]]
        "To claim it with a new account, log out and then open this invitation link again."
      Lucid.div_ [class_ $ base ["flex", "flex-col", "items-center", Tokens.gap4]] $ do
        Button.render
          ( Button.defaults
              & Button.accent
              & Button.large
              & Button.square
              & Button.pageLink logoutUrl
              & Button.with [hxGet_ logoutUrl, class_ $ base ["inline-block"]]
          )
          "LOG OUT"
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
    logoutUrl = rootLink userLinks.logoutGet

--------------------------------------------------------------------------------
-- Staff Preview Template

-- | Template shown when staff or an admin visits an invitation link.
--
-- Shows the invitation details in any status, so that staff can check the
-- link. Staff cannot claim the invitation from this page.
staffPreviewTemplate ::
  -- | Invitation with creator and claimer names
  HostInvitation.ModelWithCreator ->
  Lucid.Html ()
staffPreviewTemplate invitation =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8]] $
    Card.card Card.Auth [] $ do
      Lucid.h1_
        [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]]
        "Host invitation"
      Lucid.p_
        [class_ $ base [Tokens.fgMuted, Tokens.mb6]]
        "You are logged in as staff, so this page shows the invitation details. Log out to see the page that the host sees."
      Lucid.dl_ [class_ $ base ["grid", "grid-cols-[max-content_1fr]", "gap-x-6", "gap-y-2", Tokens.textSm, Tokens.mb8]] $ do
        detailRow "Status" $ renderStatusBadge invitation.mwcStatus
        detailRow "Recipient" $ Lucid.toHtml (display invitation.mwcRecipientEmail)
        detailRow "Schedule" $ Lucid.toHtml (renderScheduleSummary invitation.mwcScheduleData)
        detailRow "Starts" $ Lucid.toHtml (formatStartDate invitation.mwcStartsOn)
        detailRow "Created by" $ Lucid.toHtml invitation.mwcCreatedByName
        detailRow "Created" $ Lucid.toHtml (formatDateTime invitation.mwcCreatedAt)
        detailRow "Expires" $ Lucid.toHtml (formatDateTime invitation.mwcExpiresAt)
        case invitation.mwcClaimedByName of
          Just claimer -> detailRow "Claimed by" $ Lucid.toHtml claimer
          Nothing -> pure ()
      Lucid.a_
        [ Lucid.href_ invitationsUrl,
          hxGet_ invitationsUrl,
          hxSwap_ "innerHTML",
          hxTarget_ "body",
          hxPushUrl_ "true",
          class_ $ base [Tokens.linkText]
        ]
        "\x2190 All invitations"
  where
    invitationsUrl = rootLink dashboardInvitationsLinks.list

    detailRow :: Text -> Lucid.Html () -> Lucid.Html ()
    detailRow label value = do
      Lucid.dt_ [class_ $ base [Tokens.fgMuted, Tokens.fontBold]] $ Lucid.toHtml label
      Lucid.dd_ value
