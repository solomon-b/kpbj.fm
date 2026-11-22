{-# LANGUAGE QuasiQuotes #-}

-- | Render tests for the dashboard frame.
--
-- Each test renders the frame for one 'ShowContext' and checks the HTML.
module Component.DashboardFrameSpec (spec) where

--------------------------------------------------------------------------------

import API.Links (dashboardEphemeralUploadsLinks, dashboardLinks, rootLink)
import API.Types (DashboardEphemeralUploadsRoutes (..), DashboardRoutes (..))
import App.Dashboard.ShowContext (ShowContext (..), ShowSection (..))
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..), sectionUrl)
import Component.DashboardFrame qualified as DashboardFrame
import Component.Flash (FlashMessage (..))
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
      -- The Tailwind safelist in the head names the class too, so look only
      -- at the body.
      let body = snd . Text.breakOn "<body" $ renderFrame NoShow [] NavEpisodes
      body `shouldNotSatisfy` Text.isInfixOf "cursor-not-allowed"

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

    it "has the frame ids and no out-of-band attribute on a full page" $ do
      let html = renderFrame NoShow [] NavEvents
      html `shouldSatisfy` Text.isInfixOf "id=\"dashboard-nav\""
      html `shouldSatisfy` Text.isInfixOf "id=\"dashboard-top-bar\""
      html `shouldNotSatisfy` Text.isInfixOf "hx-swap-oob"

  describe "sidebar links" $ do
    it "swaps the main content from a HOST item" $ do
      let tag = anchorTag (showHref SectionEpisodes showA) (renderFrame (ForShow showA) [showA] NavEvents)
      tag `shouldSatisfy` Text.isInfixOf [i|hx-get="/#{sectionUrl SectionEpisodes showA}"|]
      tag `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      tag `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "swaps the main content from the current show route" $ do
      let tag = anchorTag (currentShowHref SectionEpisodes) (renderFrame NoShow [] NavEvents)
      tag `shouldSatisfy` Text.isInfixOf "hx-get="
      tag `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      tag `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "swaps the main content from a staff item" $ do
      let ephemeralUrl = rootLink (dashboardEphemeralUploadsLinks.list Nothing)
          tag = anchorTag [i|href="#{ephemeralUrl}"|] (renderFrame NoShow [] NavEvents)
      tag `shouldSatisfy` Text.isInfixOf "hx-get="
      tag `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      tag `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "swaps the main content from the profile settings link" $ do
      let settingsUrl = rootLink dashboardLinks.profileEditGet
          tag = anchorTag [i|href="#{settingsUrl}"|] (renderFrame NoShow [] NavEvents)
      tag `shouldSatisfy` Text.isInfixOf "hx-get="
      tag `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      tag `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "starts the new page at the top of the window" $ do
      let ephemeralUrl = rootLink (dashboardEphemeralUploadsLinks.list Nothing)
          tag = anchorTag [i|href="#{ephemeralUrl}"|] (renderFrame NoShow [] NavEvents)
      tag `shouldSatisfy` Text.isInfixOf "hx-swap=\"innerHTML show:window:top\""
      DashboardFrame.pageSwap `shouldBe` "innerHTML show:window:top"

  describe "show selector" $ do
    it "starts an HTMX request and does not set the location" $ do
      let html = renderFrame (ForShow showA) [showA, showB] NavEpisodes
          tag = openingTag "<select id=\"show-selector\"" html
      tag `shouldNotSatisfy` Text.isInfixOf "window.location"
      tag `shouldSatisfy` Text.isInfixOf "htmx.ajax"
      tag `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      tag `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "has no name, so HTMX keeps its value out of the request URL" $ do
      let html = renderFrame (ForShow showA) [showA, showB] NavEpisodes
      openingTag "<select id=\"show-selector\"" html
        `shouldNotSatisfy` Text.isInfixOf " name="

    it "starts the new page at the top of the window" $ do
      let html = renderFrame (ForShow showA) [showA, showB] NavEpisodes
      openingTag "<select id=\"show-selector\"" html
        `shouldSatisfy` Text.isInfixOf "hx-swap=\"innerHTML show:window:top\""

    it "goes back to the show of the page when its request does not succeed" $ do
      let html = renderFrame (ForShow showA) [showA, showB] NavEpisodes
          tag = openingTag "<select id=\"show-selector\"" html
      tag `shouldSatisfy` Text.isInfixOf "hx-on::after-request="
      tag `shouldSatisfy` Text.isInfixOf "option[selected]"

  describe "page requests" $ do
    it "loads the script that aborts an older page request" $ do
      let (headHtml, _body) = Text.breakOn "</head>" (renderFrame NoShow [] NavEvents)
      headHtml `shouldSatisfy` Text.isInfixOf "htmx:beforeSend"

    it "has no hx-sync attribute, because the script covers all page requests" $
      renderFrame (ForShow showA) [showA, showB] NavEpisodes
        `shouldNotSatisfy` Text.isInfixOf "hx-sync"

  describe "banner" $ do
    it "loads the script that clears the banner on a page change" $ do
      let (headHtml, _body) = Text.breakOn "</head>" (renderFrame NoShow [] NavEvents)
      headHtml `shouldSatisfy` Text.isInfixOf "htmx:beforeRequest"

    it "renders the flash message in the banner container" $
      bannerContainerContent (renderFrameWithFlash (Just created) NoShow [] NavEvents)
        `shouldSatisfy` (\content -> Text.isInfixOf "Created" content && Text.isInfixOf "Item created successfully." content)

    it "renders an empty banner container with no flash message" $
      bannerContainerContent (renderFrame NoShow [] NavEvents) `shouldBe` ""

    it "has a banner template for each banner type" $ do
      let html = renderFrame NoShow [] NavEvents
          hasTemplate name = Text.isInfixOf [i|id="banner-template-#{name}"|] html
      filter (not . hasTemplate) ["success", "error", "warning", "info" :: Text]
        `shouldBe` []

    it "does not read the flash cookie in the browser" $
      renderFrame NoShow [] NavEvents `shouldNotSatisfy` Text.isInfixOf "_flash"

  describe "browser history" $ do
    it "keeps the pages out of the HTMX history cache" $
      openingTag "<body" (renderFrame NoShow [] NavEvents)
        `shouldSatisfy` Text.isInfixOf "hx-history=\"false\""

    it "tells HTMX to load the page again when the history cache has no page" $ do
      let html = renderFrame NoShow [] NavEvents
      openingTag "<meta name=\"htmx-config\"" html
        `shouldSatisfy` Text.isInfixOf "refreshOnHistoryMiss"

    it "removes the snapshots that an older dashboard saved" $ do
      let (headHtml, _body) = Text.breakOn "</head>" (renderFrame NoShow [] NavEvents)
      headHtml `shouldSatisfy` Text.isInfixOf "localStorage.removeItem('htmx-history-cache')"

  describe "htmxResponse" $ do
    it "puts the cookie script before the content of a show page" $ do
      let html = renderHtmxResponse (ForShow showA) [showA] NavEpisodes
          (beforeContent, _content) = Text.breakOn "<p>content</p>" html
      beforeContent `shouldSatisfy` Text.isInfixOf "dashboard_show"
      beforeContent `shouldSatisfy` Text.isInfixOf "show-a"

    it "has no cookie script for a page that is not about a show" $
      renderHtmxResponse NoShow [] NavEvents
        `shouldNotSatisfy` Text.isInfixOf "dashboard_show"

    it "has the content" $
      renderHtmxResponse NoShow [] NavEvents
        `shouldSatisfy` Text.isInfixOf "<p>content</p>"

    it "sends the sidebar navigation out of band and keeps the element" $
      openingTag "<nav id=\"dashboard-nav\"" (renderHtmxResponse NoShow [] NavEvents)
        `shouldSatisfy` Text.isInfixOf "hx-swap-oob=\"innerHTML\""

    it "sends the top bar out of band" $ do
      let html = renderHtmxResponse NoShow [] NavEvents
      openingTag "<header id=\"dashboard-top-bar\"" html
        `shouldSatisfy` Text.isInfixOf "hx-swap-oob=\"true\""
      html `shouldSatisfy` Text.isInfixOf "the stats"
      html `shouldSatisfy` Text.isInfixOf "the action"

    it "has no document head" $
      renderHtmxResponse NoShow [] NavEvents
        `shouldNotSatisfy` Text.isInfixOf "<head>"

    it "selects a show that the user does not host" $ do
      let showC = sampleShow 3 "Show C" (Slug "show-c")
          html = renderHtmxResponse (ForShow showC) [showA, showB] NavEpisodes
      optionTag "show-c" html `shouldSatisfy` Text.isInfixOf "selected"

    it "keeps a hostile slug inside the cookie script" $ do
      let hostileShow = sampleShow 3 "Hostile" (Slug "x\"</script><b>")
          html = renderHtmxResponse (ForShow hostileShow) [] NavEpisodes
      html `shouldNotSatisfy` Text.isInfixOf "</script><b>"

--------------------------------------------------------------------------------

-- | Render the HTMX response of a page.
renderHtmxResponse :: ShowContext -> [Shows.Model] -> DashboardNav -> Text
renderHtmxResponse showContext selectorShows activeNav =
  LText.toStrict . Lucid.renderText $
    DashboardFrame.htmxResponse
      sampleUserMetadata
      showContext
      selectorShows
      activeNav
      (Just (Lucid.span_ "the stats"))
      (Just (Lucid.span_ "the action"))
      (Lucid.p_ "content")

-- | Render the frame with the given context, selector options, and active item.
renderFrame :: ShowContext -> [Shows.Model] -> DashboardNav -> Text
renderFrame = renderFrameWithFlash Nothing

-- | 'renderFrame' for a request with a flash message.
renderFrameWithFlash :: Maybe FlashMessage -> ShowContext -> [Shows.Model] -> DashboardNav -> Text
renderFrameWithFlash mFlash showContext selectorShows activeNav =
  LText.toStrict $ Lucid.renderText frame
  where
    frame =
      DashboardFrame.template
        Nothing
        sampleUserMetadata
        mFlash
        showContext
        selectorShows
        activeNav
        Nothing
        Nothing
        mempty

-- | A flash message.
created :: FlashMessage
created = FlashMessage Success "Created" "Item created successfully."

-- | The HTML inside the banner container of a frame. In the frame, the
-- container and its wrapper close right before the main element.
bannerContainerContent :: Text -> Text
bannerContainerContent html =
  fst (Text.breakOn "</div></div><main" afterOpeningTag)
  where
    afterId = snd (Text.breakOn "id=\"banner-container\"" html)
    afterOpeningTag = Text.drop 1 (Text.dropWhile (/= '>') afterId)

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
optionTag slugValue = openingTag [i|<option value="#{slugValue}"|]

-- | The opening tag of the first @a@ element with the given @href@ attribute.
anchorTag :: Text -> Text -> Text
anchorTag hrefAttr = openingTag [i|<a #{hrefAttr}|]

-- | The first opening tag that starts with the given text. It is empty when
-- the HTML has no such tag.
openingTag :: Text -> Text -> Text
openingTag tagStart html =
  Text.takeWhile (/= '>') afterStart
  where
    (_before, afterStart) = Text.breakOn tagStart html

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
