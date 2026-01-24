{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Slug.Edit.Get.Templates.Page
  ( editBlogPostForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks)
import API.Types
import Component.Form.Builder
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardBlogsGetUrl :: Shows.Model -> Links.URI
dashboardBlogsGetUrl showModel = Links.linkURI $ dashboardBlogsLinks.list (Shows.slug showModel) Nothing

dashboardBlogsDetailUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
dashboardBlogsDetailUrl showModel post = Links.linkURI $ dashboardBlogsLinks.detail (Shows.slug showModel) (ShowBlogPosts.id post)

dashboardBlogsEditPostUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
dashboardBlogsEditPostUrl showModel post = Links.linkURI $ dashboardBlogsLinks.editPost (Shows.slug showModel) (ShowBlogPosts.id post)

--------------------------------------------------------------------------------

-- | Edit show blog post form template using V2 FormBuilder
editBlogPostForm :: Shows.Model -> ShowBlogPosts.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
editBlogPostForm showModel post tags = do
  renderFormHeader showModel post
  renderForm config form
  where
    postUrl = [i|/#{dashboardBlogsEditPostUrl showModel post}|]
    cancelUrl = [i|/#{dashboardBlogsGetUrl showModel}|]

    tagsText = Text.intercalate ", " $ map ShowBlogTags.sbtmName tags
    isPublished = ShowBlogPosts.status post == Published

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
          value (ShowBlogPosts.title post)
          required
          minLength 3
          maxLength 200

        textareaField "content" 12 $ do
          label "Post Content"
          value (ShowBlogPosts.content post)
          required
          minLength 10
          maxLength 50000

        textField "tags" $ do
          label "Tags"
          unless (Text.null tagsText) $ value tagsText
          placeholder "music-selection, behind-the-scenes, guest-interview"
          hint "Comma separated tags (lowercase with hyphens)"
          maxLength 500

        textareaField "excerpt" 3 $ do
          label "Excerpt (Optional)"
          for_ (ShowBlogPosts.excerpt post) value
          placeholder "Short preview of your post (optional)"
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        when isPublished checked
        hint "Toggle to publish immediately"

      cancelButton cancelUrl "CANCEL"
      submitButton "SAVE CHANGES"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderFormHeader showModel post =
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300 dark:text-gray-500", Tokens.textSm]] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [class_ $ base ["text-center", "flex", Tokens.gap4]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardBlogsDetailUrl showModel post}|],
            hxGet_ [i|/#{dashboardBlogsDetailUrl showModel post}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW POST"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardBlogsGetUrl showModel}|],
            hxGet_ [i|/#{dashboardBlogsGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW BLOG"
