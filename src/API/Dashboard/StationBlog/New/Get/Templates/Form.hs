{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardStationBlogNewPostUrl :: Links.URI
dashboardStationBlogNewPostUrl = Links.linkURI dashboardStationBlogLinks.newPost

--------------------------------------------------------------------------------

-- | New blog post form template using V2 FormBuilder
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  renderFormHeader userMeta
  renderForm config form
  where
    postUrl = [i|/#{dashboardStationBlogNewPostUrl}|]
    cancelUrl = [i|/#{dashboardStationBlogGetUrl}|]

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
      -- Post Details Section
      section "POST DETAILS" $ do
        textField "title" $ do
          label "Post Title"
          placeholder "e.g. The Evolution of Industrial Ambient"
          required
          minLength 3
          maxLength 200

        textareaField "content" 12 $ do
          label "Post Content"
          placeholder "Write your blog post content here..."
          required
          minLength 10
          maxLength 50000

        imageField "hero_image" $ do
          label "Hero Image (Optional)"
          maxSize 10
          aspectRatio (16, 9)

        textareaField "excerpt" 3 $ do
          label "Excerpt (Optional)"
          placeholder "Short preview of your post (optional - will auto-generate if left blank)"
          maxLength 500

        textField "tags" $ do
          label "Tags"
          placeholder "industrial, ambient, interview, chrome-valley"
          hint "Comma separated tags"
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        hint "Toggle to publish immediately"

      submitButton "SAVE POST"
      cancelButton cancelUrl "CANCEL"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Lucid.Html ()
renderFormHeader userMeta =
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300 dark:text-gray-500", Tokens.textSm]] $ do
          Lucid.strong_ "Author: "
          Lucid.toHtml userMeta.mDisplayName
