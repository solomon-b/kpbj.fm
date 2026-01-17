{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.New.Get.Templates.Page
  ( newBlogPostForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks, showsLinks)
import API.Types
import Component.Form.Builder
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardBlogsGetUrl :: Shows.Model -> Links.URI
dashboardBlogsGetUrl showModel = Links.linkURI $ dashboardBlogsLinks.list (Shows.slug showModel) Nothing

dashboardBlogsNewPostUrl :: Shows.Model -> Links.URI
dashboardBlogsNewPostUrl showModel = Links.linkURI $ dashboardBlogsLinks.newPost (Shows.slug showModel)

showGetUrl :: Shows.Model -> Links.URI
showGetUrl showModel = Links.linkURI $ showsLinks.detail (Shows.slug showModel) Nothing

--------------------------------------------------------------------------------

-- | New show blog post form template using V2 FormBuilder
newBlogPostForm :: Shows.Model -> Lucid.Html ()
newBlogPostForm showModel = do
  renderFormHeader showModel
  renderForm config form
  where
    postUrl = [i|/#{dashboardBlogsNewPostUrl showModel}|]
    cancelUrl = [i|/#{dashboardBlogsGetUrl showModel}|]

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
          placeholder "e.g. Behind the Scenes: Selecting Music for This Week's Show"
          required
          minLength 3
          maxLength 200

        textareaField "content" 12 $ do
          label "Post Content"
          placeholder "Write your blog post content here..."
          required
          minLength 10
          maxLength 50000

        textField "tags" $ do
          label "Tags"
          placeholder "music-selection, behind-the-scenes, guest-interview"
          hint "Comma separated tags (lowercase with hyphens)"
          maxLength 500

        textareaField "excerpt" 3 $ do
          label "Excerpt (Optional)"
          placeholder "Short preview of your post (optional - will auto-generate if left blank)"
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        hint "Toggle to publish immediately"

      cancelButton cancelUrl "CANCEL"
      submitButton "SAVE POST"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> Lucid.Html ()
renderFormHeader showModel =
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [class_ $ base ["text-center", "flex", Tokens.gap4]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showModel}|],
            hxGet_ [i|/#{showGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardBlogsGetUrl showModel}|],
            hxGet_ [i|/#{dashboardBlogsGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW BLOG"
