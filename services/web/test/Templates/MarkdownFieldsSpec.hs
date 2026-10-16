-- | Render tests for the long text fields that support Markdown.
--
-- Full views must render Markdown as HTML. Summaries and row snippets must
-- strip Markdown to plain text. Each test renders a template with
-- @**bold**@ in the field and checks the HTML output.
module Templates.MarkdownFieldsSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.BlogPostRow qualified as DashboardBlogPostRow
import API.Dashboard.Blogs.Slug.Get.Templates.Page qualified as DashboardShowBlogPage
import API.Dashboard.Episodes.Get.Templates.EpisodeRow qualified as DashboardEpisodeRow
import API.Dashboard.Episodes.Slug.Get.Templates.Page qualified as DashboardEpisodePage
import API.Dashboard.Events.Slug.Get.Templates.Page qualified as DashboardEventPage
import API.Dashboard.Shows.Slug.Get.Templates.Episode qualified as DashboardShowEpisode
import API.Dashboard.Shows.Slug.Get.Templates.ShowHeader qualified as DashboardShowHeader
import API.Dashboard.StationBlog.Slug.Get.Templates.Page qualified as DashboardStationBlogPage
import API.Shows.Slug.Episode.Get.Templates.Page qualified as PublicEpisodePage
import API.Shows.Slug.Get.Templates.ShowHeader qualified as PublicShowHeader
import API.Store.Products.Slug.Get.Templates qualified as PublicProductPage
import Component.Card.BlogPost (BlogPostCardData (..), renderBlogPostCard)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time (UTCTime (..), fromGregorian)
import Domain.Types.Cents (Cents (..))
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (mkSlug)
import Domain.Types.StorageBackend (StorageBackend (..), defaultLocalConfig)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Markdown in long text fields" $ do
  describe "public site renders Markdown" $ do
    it "renders the show description on the show page" $ do
      let html = PublicShowHeader.renderShowHeader backend (sampleShow (Just boldMarkdown)) [] [] []
      render html `shouldSatisfy` rendersBold

    it "renders the episode description on the episode page" $ do
      let html = PublicEpisodePage.template backend (sampleShow Nothing) (sampleEpisode (Just boldMarkdown)) [] []
      render html `shouldSatisfy` rendersBold

    it "renders nothing for an empty episode description" $ do
      let withEmpty = render (PublicEpisodePage.template backend (sampleShow Nothing) (sampleEpisode (Just "")) [] [])
          withNothing = render (PublicEpisodePage.template backend (sampleShow Nothing) (sampleEpisode Nothing) [] [])
      withEmpty `shouldBe` withNothing

    it "renders the product description on the product page" $ do
      let html = PublicProductPage.template backend (sampleProduct boldMarkdown) [] [] [] [] []
      render html `shouldSatisfy` rendersBold

  describe "blog post card strips Markdown" $ do
    it "strips Markdown from the excerpt" $ do
      let card = sampleCard (Just boldMarkdown) "Unused content"
      render (renderBlogPostCard backend card) `shouldSatisfy` stripsBold

    it "strips Markdown from the content when there is no excerpt" $ do
      let card = sampleCard Nothing boldMarkdown
      render (renderBlogPostCard backend card) `shouldSatisfy` stripsBold

    it "strips Markdown before it truncates long content" $ do
      -- The link starts before character 200 and ends after it.
      let longContent = Text.replicate 190 "a" <> " [link text](https://kpbj.fm/a-very-long-url)"
          rendered = render (renderBlogPostCard backend (sampleCard Nothing longContent))
      rendered `shouldSatisfy` LText.isInfixOf "link text"
      rendered `shouldNotSatisfy` LText.isInfixOf "https://kpbj.fm/a-very-long-url"

  describe "dashboard previews render Markdown" $ do
    it "renders the show description on the show page" $ do
      let html = DashboardShowHeader.renderShowHeader backend (sampleShow (Just boldMarkdown)) [] [] []
      render html `shouldSatisfy` rendersBold

    it "renders the latest episode description on the show page" $ do
      let html = DashboardShowEpisode.renderLatestEpisode backend (sampleShow Nothing) (sampleEpisode (Just boldMarkdown)) []
      render html `shouldSatisfy` rendersBold

    it "renders the episode description on the episode page" $ do
      let html = DashboardEpisodePage.template backend sampleUserMetadata (sampleShow Nothing) (sampleEpisode (Just boldMarkdown)) [] []
      render html `shouldSatisfy` rendersBold

    it "renders the station blog content" $ do
      let html = DashboardStationBlogPage.template (sampleBlogPost Nothing boldMarkdown) [] Nothing
      render html `shouldSatisfy` rendersBold

    it "renders the show blog content" $ do
      let html = DashboardShowBlogPage.template sampleUserMetadata (sampleShow Nothing) (sampleShowBlogPost Nothing boldMarkdown) []
      render html `shouldSatisfy` rendersBold

    it "renders the event description" $ do
      let html = DashboardEventPage.template (sampleEvent boldMarkdown) Nothing
      render html `shouldSatisfy` rendersBold

  describe "dashboard summaries strip Markdown" $ do
    it "strips Markdown from the episode card snippet on the show page" $ do
      let html = DashboardShowEpisode.renderEpisodeCard backend (sampleShow Nothing) (sampleEpisode (Just boldMarkdown))
      render html `shouldSatisfy` stripsBold

    it "strips Markdown from the station blog excerpt" $ do
      let html = DashboardStationBlogPage.template (sampleBlogPost (Just boldMarkdown) "Plain content") [] Nothing
      render html `shouldSatisfy` stripsBold

    it "strips Markdown from the show blog excerpt" $ do
      let html = DashboardShowBlogPage.template sampleUserMetadata (sampleShow Nothing) (sampleShowBlogPost (Just boldMarkdown) "Plain content") []
      render html `shouldSatisfy` stripsBold

    it "strips Markdown from the episode row snippet" $ do
      let html = DashboardEpisodeRow.renderEpisodeTableRow sampleUserMetadata (sampleShow Nothing) (sampleEpisode (Just boldMarkdown))
      render html `shouldSatisfy` stripsBold

    it "strips Markdown from the show blog row snippet" $ do
      let html = DashboardBlogPostRow.renderBlogPostTableRow (sampleShow Nothing) (sampleShowBlogPost Nothing boldMarkdown)
      render html `shouldSatisfy` stripsBold

--------------------------------------------------------------------------------
-- Assertions

-- | Markdown input that every test puts in the field.
boldMarkdown :: Text
boldMarkdown = "Some **bold** words"

render :: Lucid.Html () -> LText.Text
render = Lucid.renderText

-- | The output has a strong element.
rendersBold :: LText.Text -> Bool
rendersBold = LText.isInfixOf "<strong"

-- | The output has the word but no Markdown markers and no strong element.
stripsBold :: LText.Text -> Bool
stripsBold rendered =
  LText.isInfixOf "Some bold words" rendered
    && not (LText.isInfixOf "**" rendered)
    && not (LText.isInfixOf "<strong" rendered)

--------------------------------------------------------------------------------
-- Sample models

backend :: StorageBackend
backend = LocalStorage defaultLocalConfig

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 1 1) 0

sampleShow :: Maybe Text -> Shows.Model
sampleShow description =
  Shows.Show
    { Shows.id = Shows.Id 1,
      Shows.title = "Sample Show",
      Shows.slug = mkSlug "Sample Show",
      Shows.description = description,
      Shows.logoUrl = Nothing,
      Shows.status = Shows.Active,
      Shows.createdAt = sampleTime,
      Shows.updatedAt = sampleTime,
      Shows.deletedAt = Nothing
    }

sampleEpisode :: Maybe Text -> Episodes.Model
sampleEpisode description =
  Episodes.Episode
    { Episodes.id = Episodes.Id 1,
      Episodes.showId = Shows.Id 1,
      Episodes.description = description,
      Episodes.episodeNumber = Episodes.EpisodeNumber 1,
      Episodes.audioFilePath = Nothing,
      Episodes.audioFileSize = Nothing,
      Episodes.audioMimeType = Nothing,
      Episodes.durationSeconds = Nothing,
      Episodes.artworkUrl = Nothing,
      Episodes.scheduleTemplateId = Nothing,
      Episodes.airDate = Nothing,
      Episodes.publishedAt = Nothing,
      Episodes.deletedAt = Nothing,
      Episodes.createdBy = User.Id 1,
      Episodes.createdAt = sampleTime,
      Episodes.updatedAt = sampleTime
    }

sampleProduct :: Text -> Products.Model
sampleProduct description =
  Products.Product
    { Products.pId = Products.Id 1,
      Products.pName = "Tote Bag",
      Products.pSlug = mkSlug "Tote Bag",
      Products.pDescription = description,
      Products.pBasePriceCents = Cents 2000,
      Products.pWeightOz = 8,
      Products.pCategory = Nothing,
      Products.pInventoryCount = 5,
      Products.pIsActive = True,
      Products.pSortOrder = 0,
      Products.pCreatedAt = sampleTime,
      Products.pUpdatedAt = sampleTime
    }

sampleBlogPost :: Maybe Text -> Text -> BlogPosts.Model
sampleBlogPost excerpt content =
  BlogPosts.BlogPost
    { BlogPosts.bpmId = BlogPosts.Id 1,
      BlogPosts.bpmTitle = "Station News",
      BlogPosts.bpmSlug = mkSlug "Station News",
      BlogPosts.bpmContent = content,
      BlogPosts.bpmExcerpt = excerpt,
      BlogPosts.bpmHeroImageUrl = Nothing,
      BlogPosts.bpmAuthorId = User.Id 1,
      BlogPosts.bpmStatus = Published,
      BlogPosts.bpmPublishedAt = Just sampleTime,
      BlogPosts.bpmCreatedAt = sampleTime,
      BlogPosts.bpmUpdatedAt = sampleTime
    }

sampleShowBlogPost :: Maybe Text -> Text -> ShowBlogPosts.Model
sampleShowBlogPost excerpt content =
  ShowBlogPosts.ShowBlogPost
    { ShowBlogPosts.id = ShowBlogPosts.Id 1,
      ShowBlogPosts.showId = Shows.Id 1,
      ShowBlogPosts.title = "Show News",
      ShowBlogPosts.slug = mkSlug "Show News",
      ShowBlogPosts.content = content,
      ShowBlogPosts.excerpt = excerpt,
      ShowBlogPosts.authorId = User.Id 1,
      ShowBlogPosts.status = Published,
      ShowBlogPosts.publishedAt = Just sampleTime,
      ShowBlogPosts.createdAt = sampleTime,
      ShowBlogPosts.updatedAt = sampleTime
    }

sampleEvent :: Text -> Events.Model
sampleEvent description =
  Events.Event
    { Events.emId = Events.Id 1,
      Events.emTitle = "Spring Concert",
      Events.emSlug = mkSlug "Spring Concert",
      Events.emDescription = description,
      Events.emStartsAt = sampleTime,
      Events.emEndsAt = sampleTime,
      Events.emLocationName = "Community Center",
      Events.emLocationAddress = "123 Main St",
      Events.emStatus = Events.Published,
      Events.emAuthorId = User.Id 1,
      Events.emPosterImageUrl = Nothing,
      Events.emTicketUrl = Nothing,
      Events.emFeaturedOnHomepage = False,
      Events.emCreatedAt = sampleTime,
      Events.emUpdatedAt = sampleTime
    }

sampleUserMetadata :: UserMetadata.Model
sampleUserMetadata =
  UserMetadata.Model
    { UserMetadata.mId = UserMetadata.Id 1,
      UserMetadata.mUserId = User.Id 1,
      UserMetadata.mDisplayName = mkDisplayNameUnsafe "Test Host",
      UserMetadata.mFullName = mkFullNameUnsafe "Test Host",
      UserMetadata.mAvatarUrl = Nothing,
      UserMetadata.mUserRole = UserMetadata.Host,
      UserMetadata.mColorScheme = UserMetadata.Automatic,
      UserMetadata.mTheme = UserMetadata.DefaultTheme,
      UserMetadata.mSuspensionStatus = UserMetadata.NotSuspended
    }

sampleCard :: Maybe Text -> Text -> BlogPostCardData
sampleCard excerpt content =
  BlogPostCardData
    { cardUrl = "/blog/1/sample",
      cardTitle = "Sample Post",
      cardExcerpt = excerpt,
      cardContent = content,
      cardHeroImageUrl = Nothing,
      cardAuthorName = Nothing,
      cardPublishedAt = Nothing,
      cardCurrentTime = Nothing
    }
