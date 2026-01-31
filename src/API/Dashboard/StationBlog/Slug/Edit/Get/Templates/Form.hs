{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardStationBlogDetailGetUrl :: BlogPosts.Id -> Slug -> Links.URI
dashboardStationBlogDetailGetUrl postId slug = Links.linkURI $ dashboardStationBlogLinks.detail postId slug

dashboardStationBlogEditPostUrl :: BlogPosts.Id -> Slug -> Links.URI
dashboardStationBlogEditPostUrl postId slug = Links.linkURI $ dashboardStationBlogLinks.editPost postId slug

--------------------------------------------------------------------------------

-- | Blog post edit template using V2 FormBuilder
template :: StorageBackend -> BlogPosts.Model -> [BlogTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template backend blogPost tags userMeta = do
  renderFormHeader blogPost userMeta postBackUrl
  renderForm config form
  where
    postId = blogPost.bpmId
    postSlug = blogPost.bpmSlug
    postBackUrl = dashboardStationBlogDetailGetUrl postId postSlug
    postEditUrl = dashboardStationBlogEditPostUrl postId postSlug
    tagsText = Text.intercalate ", " $ map (\t -> t.btmName) tags
    imageUrl = maybe "" (buildMediaUrl backend) blogPost.bpmHeroImageUrl

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = [i|/#{postEditUrl}|],
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
          value blogPost.bpmTitle
          required
          minLength 3
          maxLength 200

        textareaField "content" 12 $ do
          label "Post Content"
          placeholder "Write your blog post content here..."
          value blogPost.bpmContent
          required
          minLength 10
          maxLength 50000

        imageField "hero_image" $ do
          label "Hero Image"
          maxSize 10
          aspectRatio (16, 9)
          currentFile imageUrl

        textareaField "excerpt" 3 $ do
          label "Excerpt (Optional)"
          placeholder "Short preview of your post (optional - will auto-generate if left blank)"
          for_ blogPost.bpmExcerpt value
          maxLength 500

        textField "tags" $ do
          label "Tags"
          placeholder "industrial, ambient, interview, chrome-valley"
          hint "Comma separated tags"
          unless (Text.null tagsText) $ value tagsText
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        when (blogPost.bpmStatus == Published) checked
        hint "Toggle to publish immediately"

      cancelButton [i|/#{postBackUrl}|] "CANCEL"
      submitButton "UPDATE POST"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: BlogPosts.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
renderFormHeader blogPost userMeta postBackUrl =
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300 dark:text-gray-500", Tokens.textSm]] $ do
          Lucid.strong_ "Post: "
          Lucid.toHtml blogPost.bpmTitle
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [class_ $ base ["space-x-4"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{postBackUrl}|],
            hxGet_ [i|/#{postBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "<- BACK TO POST"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardStationBlogGetUrl}|],
            hxGet_ [i|/#{dashboardStationBlogGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW ALL POSTS"
