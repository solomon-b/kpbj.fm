{-# LANGUAGE QuasiQuotes #-}

-- | Render tests for the dashboard frame.
--
-- Each test renders the frame for one 'ShowContext' and checks the HTML.
module Component.DashboardFrameSpec (spec) where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks, rootLink)
import API.Types (DashboardRoutes (..))
import App.Dashboard.ShowContext (ShowContext (..), ShowSection (..))
import Component.DashboardFrame (DashboardNav (..), sectionUrl)
import Component.DashboardFrame qualified as DashboardFrame
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time (UTCTime (..), fromGregorian)
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.DashboardFrame" $ do
  describe "NoShow" $ do
    it "links each HOST item to the current show route" $ do
      let html = renderFrame NoShow [] NavEvents
      html `shouldSatisfy` Text.isInfixOf (currentShowHref SectionEpisodes)
      html `shouldSatisfy` Text.isInfixOf (currentShowHref SectionPromos)
      html `shouldSatisfy` Text.isInfixOf (currentShowHref SectionSettings)

    it "has no show selector" $ do
      let html = renderFrame NoShow [] NavEvents
      html `shouldNotSatisfy` Text.isInfixOf "id=\"show-selector\""

    it "has no cookie script" $ do
      let html = renderFrame NoShow [] NavEvents
      html `shouldNotSatisfy` Text.isInfixOf "dashboard_show"

    it "has no disabled navigation item" $ do
      let html = renderFrame NoShow [] NavEpisodes
      html `shouldNotSatisfy` Text.isInfixOf "cursor-not-allowed"

  describe "ForShow" $ do
    it "shows the selector with the current show selected on a show page" $ do
      let html = renderFrame (ForShow showB) [showA, showB] NavEpisodes
      html `shouldSatisfy` Text.isInfixOf "id=\"show-selector\""
      html `shouldSatisfy` Text.isInfixOf (showHref SectionEpisodes showB)
      html `shouldSatisfy` Text.isInfixOf "dashboard_show"
      optionTag "show-b" html `shouldSatisfy` Text.isInfixOf "selected"
      optionTag "show-a" html `shouldNotSatisfy` Text.isInfixOf "selected"

    it "adds a current show that is not in the list and selects it" $ do
      let showC = sampleShow 3 "Show C" (Slug "show-c")
          html = renderFrame (ForShow showC) [showA, showB] NavEpisodes
      optionTag "show-c" html `shouldSatisfy` Text.isInfixOf "selected"
      optionTag "show-a" html `shouldNotSatisfy` Text.isInfixOf "selected"
      optionTag "show-b" html `shouldNotSatisfy` Text.isInfixOf "selected"

    it "shows the title of a current show when the list is empty" $ do
      let html = renderFrame (ForShow showA) [] NavEpisodes
      html `shouldSatisfy` Text.isInfixOf "Show A"

    it "hides the selector on a page that is not about a show" $ do
      let html = renderFrame (ForShow showA) [showA] NavShows
      html `shouldNotSatisfy` Text.isInfixOf "id=\"show-selector\""
      html `shouldSatisfy` Text.isInfixOf "dashboard_show"
      html `shouldSatisfy` Text.isInfixOf (showHref SectionSettings showA)

    it "keeps a hostile slug inside the cookie script" $ do
      let hostileShow = sampleShow 3 "Hostile" (Slug "x\"</script><b>")
          html = renderFrame (ForShow hostileShow) [hostileShow] NavShows
      html `shouldSatisfy` Text.isInfixOf "dashboard_show"
      html `shouldSatisfy` Text.isInfixOf "<\\/script><b>"
      html `shouldNotSatisfy` Text.isInfixOf "</script><b>"

  describe "contentOnly" $ do
    it "puts the cookie script before the content of a show page" $ do
      let html = renderContentOnly (ForShow showA)
      html `shouldSatisfy` Text.isInfixOf "dashboard_show"
      html `shouldSatisfy` Text.isInfixOf "show-a"
      html `shouldSatisfy` Text.isSuffixOf "<p>content</p>"

    it "gives the content alone for a page that is not about a show" $ do
      renderContentOnly NoShow `shouldBe` "<p>content</p>"

--------------------------------------------------------------------------------

-- | Render the HTMX response of a page with the given context.
renderContentOnly :: ShowContext -> Text
renderContentOnly showContext =
  LText.toStrict . Lucid.renderText $
    DashboardFrame.contentOnly showContext (Lucid.p_ "content")

-- | Render the frame with the given context, selector options, and active item.
renderFrame :: ShowContext -> [Shows.Model] -> DashboardNav -> Text
renderFrame showContext selectorShows activeNav =
  LText.toStrict $ Lucid.renderText frame
  where
    frame =
      DashboardFrame.template
        Nothing
        sampleUserMetadata
        showContext
        selectorShows
        activeNav
        Nothing
        Nothing
        mempty

-- | The @href@ attribute of the current show route for a section.
currentShowHref :: ShowSection -> Text
currentShowHref section =
  [i|href="#{currentShowUrl}"|]
  where
    currentShowUrl = rootLink (dashboardLinks.currentShow section)

-- | The @href@ attribute of a section page for one show.
showHref :: ShowSection -> Shows.Model -> Text
showHref section showModel =
  [i|href="/#{sectionUrl section showModel}"|]

-- | The opening tag of the selector option whose value is the slug.
optionTag :: Text -> Text -> Text
optionTag slugValue html =
  Text.takeWhile (/= '>') afterStart
  where
    (_before, afterStart) = Text.breakOn [i|<option value="#{slugValue}"|] html

--------------------------------------------------------------------------------

showA :: Shows.Model
showA = sampleShow 1 "Show A" (Slug "show-a")

showB :: Shows.Model
showB = sampleShow 2 "Show B" (Slug "show-b")

sampleShow :: Int64 -> Text -> Slug -> Shows.Model
sampleShow showId title slug =
  Shows.Show
    { Shows.id = Shows.Id showId,
      Shows.title = title,
      Shows.slug = slug,
      Shows.description = Nothing,
      Shows.logoUrl = Nothing,
      Shows.status = Shows.Active,
      Shows.createdAt = sampleTime,
      Shows.updatedAt = sampleTime,
      Shows.deletedAt = Nothing
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

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 1 1) 0
