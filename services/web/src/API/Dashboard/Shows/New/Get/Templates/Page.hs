{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Component.ScheduleEditor (ScheduleEditorData (..), renderScheduleEditor)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ apiLinks.dashboard.admin.shows.list Nothing Nothing Nothing

dashboardShowsNewPostUrl :: Links.URI
dashboardShowsNewPostUrl = Links.linkURI apiLinks.dashboard.admin.shows.newPost

--------------------------------------------------------------------------------

-- | New show form template using V2 FormBuilder
template :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
template eligibleHosts = do
  renderFormHeader
  renderForm config form
  where
    postUrl :: Text
    postUrl = [i|/#{dashboardShowsNewPostUrl}|]

    backUrl :: Text
    backUrl = [i|/#{dashboardShowsGetUrl}|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      -- Basic Information Section
      section "BASIC INFORMATION" $ do
        textField "title" $ do
          label "Show Title"
          placeholder "e.g. Industrial Depths"
          required
          minLength 3
          maxLength 200

        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe your show. What kind of music do you play? What's your show's vibe?"
          maxLength 5000

        textField "tags" $ do
          label "Tags"
          placeholder "e.g. Techno, Ambient, Experimental, Hip-Hop"
          hint "Comma-separated tags for categorization and filtering"
          maxLength 500

      -- Schedule & Settings Section
      section "SCHEDULE & SETTINGS" $ do
        selectField "status" $ do
          label "Show Status"
          hint "Active shows appear on the shows page"
          required
          addOptionSelected "active" "Active"
          addOption "inactive" "Inactive"

      -- Artwork & Branding Section
      section "ARTWORK & BRANDING" $ do
        imageField "logo_file" $ do
          label "Logo Image"
          maxSize 10
          aspectRatio (4, 3)

      -- Hosts Section
      section "HOSTS" $ do
        plain $ renderHostsMultiSelect eligibleHosts

      -- Schedule Section
      section "SCHEDULE" $ do
        plain $ do
          Lucid.p_
            [class_ $ base [T.textSm, T.fgMuted, T.mb4]]
            "Set the recurring schedule for this show."
          renderScheduleEditor (ScheduleEditorData "[]" "")

      cancelButton backUrl "CANCEL"
      submitButton "CREATE SHOW"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Lucid.Html ()
renderFormHeader =
  Lucid.section_ [class_ $ base [T.bgInverse, T.fgInverse, T.p6, T.mb8, T.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [T.text2xl, T.fontBold, T.mb2]] "CREATE NEW SHOW"
        Lucid.div_
          [class_ $ base [T.fgMuted, T.textSm]]
          "Add a new show to the station"
      Lucid.div_ $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
            hxGet_ [i|/#{dashboardShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [T.infoText, "hover:opacity-80", T.textSm, "underline"]
          ]
          "← BACK TO SHOWS"

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts = do
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [class_ $ base [T.textXs, T.fgMuted, T.mb2]] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [class_ $ base [T.mb2]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          class_ $ base [T.fullWidth, T.p3, T.border2, T.borderMuted, T.bgMain, T.fgPrimary, "font-mono"],
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [class_ $ base [T.bgAlt, T.border2, T.borderMuted]] $ do
      -- Header
      Lucid.div_ [class_ $ base [T.bgInverse, T.fgInverse, "border-b", T.borderMuted, T.p3, T.fontBold, T.textSm]] $
        Lucid.toHtml ("AVAILABLE HOSTS (" <> show (length eligibleHosts) <> ")")

      -- Scrollable host list
      Lucid.div_ [Lucid.class_ "max-h-64 overflow-y-auto"] $
        mapM_ renderHostOption eligibleHosts

renderHostOption :: UserMetadata.UserWithMetadata -> Lucid.Html ()
renderHostOption user =
  let userId = user.uwmUserId
      displayName = display user.uwmDisplayName
      email = display user.uwmEmail
      roleText = display user.uwmUserRole
      userIdText = display userId
      -- Alpine.js filter condition - check if search matches name or email
      filterCondition =
        [i|search === '' || '#{displayName}'.toLowerCase().includes(search.toLowerCase()) || '#{email}'.toLowerCase().includes(search.toLowerCase())|]
   in Lucid.div_
        [ class_ $ base ["border-b", T.borderMuted, T.p3, T.hoverBg, "cursor-pointer"],
          xShow_ filterCondition,
          xBindClass_ [i|{ '#{T.infoBg}': $refs.host_#{userIdText}?.checked }|]
        ]
        $ do
          Lucid.div_ [class_ $ base ["flex", "items-center"]] $ do
            Lucid.input_
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], class_ $ base ["flex-1", "cursor-pointer"]] $ do
              Lucid.div_ [class_ $ base [T.fontBold]] $ Lucid.toHtml displayName
              Lucid.div_ [class_ $ base [T.textSm, T.fgMuted]] $
                Lucid.toHtml (email <> " • " <> roleText)
