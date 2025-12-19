{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

import API.Links (apiLinks, blogLinks, dashboardLinks, eventsLinks, scheduleLink, showsLinks, userLinks)
import API.Types
import Component.Banner (bannerContainerId)
import Control.Monad.Catch (MonadThrow)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design.Tokens qualified as Tokens
import Domain.Types.DisplayName (DisplayName)
import Effects.Database.Tables.UserMetadata (SuspensionStatus (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xModel_, xOnClick_, xOnInput_, xRef_, xText_)
import Lucid.Responsive (cls)
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

rootGetUrl :: Link.URI
rootGetUrl = Link.linkURI apiLinks.rootGet

aboutGetUrl :: Link.URI
aboutGetUrl = Link.linkURI apiLinks.aboutGet

donateGetUrl :: Link.URI
donateGetUrl = Link.linkURI apiLinks.donateGet

blogGetUrl :: Link.URI
blogGetUrl = Link.linkURI $ blogLinks.list Nothing Nothing

eventsGetUrl :: Link.URI
eventsGetUrl = Link.linkURI $ eventsLinks.list Nothing Nothing

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI $ userLinks.loginGet Nothing Nothing

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI $ userLinks.registerGet Nothing Nothing Nothing

userLogoutGetUrl :: Link.URI
userLogoutGetUrl = Link.linkURI userLinks.logoutGet

dashboardGetUrl :: Link.URI
dashboardGetUrl = Link.linkURI dashboardLinks.episodesRedirect

scheduleGetUrl :: Link.URI
scheduleGetUrl = Link.linkURI $ scheduleLink Nothing

showsGetUrl :: Link.URI
showsGetUrl = Link.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

newtype UserInfo = UserInfo {userDisplayName :: DisplayName}

musicPlayer :: Lucid.Html ()
musicPlayer =
  Lucid.div_
    [ Lucid.class_ $ cls [Tokens.px6, "text-center"],
      xData_ playerData
    ]
    $ do
      Lucid.audio_
        [ xRef_ "audio",
          Lucid.preload_ "none"
        ]
        mempty
      Lucid.div_ [Lucid.class_ $ cls ["inline-flex", "items-center", Tokens.gap4, Tokens.textSm, "font-mono"]] $ do
        Lucid.a_
          [ Lucid.href_ "#",
            Lucid.class_ "hover:text-gray-600 cursor-pointer",
            xOnClick_ "toggle()",
            xText_ "isPlaying ? '[ PAUSE ]' : '[ PLAY ]'"
          ]
          "[ PLAY ]"
        Lucid.span_ [Lucid.class_ "text-gray-500"] "|"
        Lucid.div_ [xText_ "currentShow || 'NOW PLAYING: KPBJ 95.9 FM'"] "NOW PLAYING: KPBJ 95.9 FM"
        Lucid.span_ [Lucid.class_ "text-gray-500"] "|"
        Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", Tokens.gap2]] $ do
          Lucid.span_ "VOL:"
          Lucid.input_
            [ Lucid.type_ "range",
              Lucid.min_ "0",
              Lucid.max_ "100",
              xModel_ "volume",
              xOnInput_ "setVolume($event.target.value)",
              Lucid.class_ "w-20 h-1"
            ]
  where
    playerData =
      [i|{
      playerId: 'navbar-player',
      isPlaying: false,
      volume: 80,
      streamUrl: 'https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3',
      metadataUrl: 'https://kpbj.hasnoskills.com/listen/kpbj_test_station/status-json.xsl',

      // Metadata
      currentShow: '',
      currentTrack: '',
      currentArtist: '',
      errorMessage: '',
      metadataInterval: null,

      init() {
        this.setVolume(this.volume);
        this.fetchMetadata(); // Initial fetch
      },

      toggle() {
        this.isPlaying ? this.pause() : this.play();
      },

      play() {
        console.log('Navbar player: Starting...');

        // Pause all other players
        pauseOtherPlayers(this.playerId);

        const audio = this.$refs.audio;

        // Load source if needed
        if (!audio.src || audio.src === '') {
          audio.src = this.streamUrl;
        }

        audio.volume = this.volume / 100;

        audio.play()
          .then(() => {
            console.log('Navbar player: Playing');
            this.isPlaying = true;
            this.startMetadataPolling();
          })
          .catch((error) => {
            console.error('Navbar player failed:', error);
            this.errorMessage = 'Failed to start stream: ' + error.message;
            setTimeout(() => this.errorMessage = '', 5000);
          });
      },

      pause() {
        console.log('Navbar player: Pausing');
        const audio = this.$refs.audio;
        audio.pause();
        this.isPlaying = false;
      },

      setVolume(value) {
        this.volume = value;
        const audio = this.$refs.audio;
        if (audio) {
          audio.volume = value / 100;
        }
      },

      async fetchMetadata() {
        try {
          // Try multiple approaches to get stream metadata
          const response = await fetch(this.streamUrl, {
            method: 'HEAD',
            headers: {
              'Icy-MetaData': '1'
            }
          });

          const icyTitle = response.headers.get('icy-name') || response.headers.get('icy-description');
          const icyGenre = response.headers.get('icy-genre');

          console.log(icyTitle);

          if (icyTitle) {
            this.currentShow = icyTitle;
            this.errorMessage = '';
            console.log('Fetched metadata:', icyTitle);
          } else {
            // Fallback: parse common metadata formats
            this.parseStreamTitle('KPBJ 95.9 FM - Community Radio');
          }
        } catch (error) {
          console.log('Metadata fetch failed, using default');
          this.currentShow = 'KPBJ 95.9 FM';
          this.currentTrack = 'Live Community Radio';
        }
      },

      parseStreamTitle(title) {
        if (!title) return;

        // Common patterns: "Artist - Track", "Show: Artist - Track", etc.
        if (title.includes(' - ')) {
          const parts = title.split(' - ');
          if (parts.length >= 2) {
            this.currentArtist = parts[0].trim();
            this.currentTrack = parts.slice(1).join(' - ').trim();
          }
        } else if (title.includes(':')) {
          const parts = title.split(':');
          this.currentShow = parts[0].trim();
          if (parts[1]) {
            this.parseStreamTitle(parts[1].trim());
          }
        } else {
          this.currentTrack = title.trim();
        }
      },

      startMetadataPolling() {
        // Poll for metadata every 30 seconds
        this.metadataInterval = setInterval(() => {
          this.fetchMetadata();
        }, 30000);
      },

      stopMetadataPolling() {
        if (this.metadataInterval) {
          clearInterval(this.metadataInterval);
          this.metadataInterval = null;
        }
      }
    }|]

-- | JavaScript function to coordinate multiple independent audio players
playerScript :: Text
playerScript =
  [i|
    // Global state to track which player is currently active
    window.currentActivePlayer = null;

    // Pause all other players when a new one starts
    function pauseOtherPlayers(currentPlayerId) {
      // Get all Alpine components on the page that have a pause method
      document.querySelectorAll('[x-data]').forEach(el => {
        const data = Alpine.$data(el);
        if (data && data.playerId !== currentPlayerId && data.isPlaying && typeof data.pause === 'function') {
          data.pause();
        }
      });
    }
  |]

-- | JavaScript to display banner from URL query parameters on page load
--
-- Reads _banner, _title, and _msg query params, displays the banner,
-- then cleans up the URL using history.replaceState.
bannerFromUrlScript :: Text
bannerFromUrlScript =
  "\
  \(function() {\
  \  const params = new URLSearchParams(window.location.search);\
  \  const bannerType = params.get('_banner');\
  \  const title = params.get('_title');\
  \  const message = params.get('_msg');\
  \  if (bannerType && title && message) {\
  \    const styles = {\
  \      success: { bg: 'bg-green-100', border: 'border-green-600', title: 'text-green-800', msg: 'text-green-700', dismiss: 'text-green-600 hover:text-green-800', icon: '\\u2713' },\
  \      error: { bg: 'bg-red-100', border: 'border-red-600', title: 'text-red-800', msg: 'text-red-700', dismiss: 'text-red-600 hover:text-red-800', icon: '\\u2715' },\
  \      warning: { bg: 'bg-yellow-100', border: 'border-yellow-600', title: 'text-yellow-800', msg: 'text-yellow-700', dismiss: 'text-yellow-600 hover:text-yellow-800', icon: '\\u26A0' },\
  \      info: { bg: 'bg-blue-100', border: 'border-blue-600', title: 'text-blue-800', msg: 'text-blue-700', dismiss: 'text-blue-600 hover:text-blue-800', icon: '\\u2139' }\
  \    };\
  \    const style = styles[bannerType] || styles.info;\
  \    const container = document.getElementById('banner-container');\
  \    if (container) {\
  \      container.innerHTML = '<div id=\"banner\" class=\"' + style.bg + ' border-2 ' + style.border + ' p-4 mb-6 w-full\">' +\
  \        '<div class=\"flex items-center justify-between\">' +\
  \          '<div class=\"flex items-center gap-3\">' +\
  \            '<span class=\"text-2xl\">' + style.icon + '</span>' +\
  \            '<div>' +\
  \              '<h3 class=\"font-bold ' + style.title + '\">' + decodeURIComponent(title) + '</h3>' +\
  \              '<p class=\"text-sm ' + style.msg + '\">' + decodeURIComponent(message) + '</p>' +\
  \            '</div>' +\
  \          '</div>' +\
  \          '<button onclick=\"this.closest(\\'#banner\\').remove()\" class=\"' + style.dismiss + ' font-bold text-xl\">\\u00D7</button>' +\
  \        '</div>' +\
  \      '</div>';\
  \    }\
  \    params.delete('_banner');\
  \    params.delete('_title');\
  \    params.delete('_msg');\
  \    const newUrl = params.toString() ? window.location.pathname + '?' + params.toString() : window.location.pathname;\
  \    window.history.replaceState({}, '', newUrl);\
  \  }\
  \})();"

authWidget :: Maybe UserMetadata.Model -> Lucid.Html ()
authWidget mUser =
  Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap4, "items-center", Tokens.textSm, Tokens.textGray600]] $ do
    case mUser of
      Nothing -> do
        Lucid.a_ [Lucid.href_ [i|/#{userLoginGetUrl}|], hxGet_ [i|/#{userLoginGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Login"
        Lucid.a_ [Lucid.href_ [i|/#{userRegisterGetUrl}|], hxGet_ [i|/#{userRegisterGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Sign Up"
      Just user -> do
        Lucid.span_ [Lucid.class_ "text-gray-400"] "•"
        Lucid.span_ [Lucid.class_ $ cls [Tokens.textGray800, Tokens.fontBold]] ("Welcome, " <> Lucid.toHtml user.mDisplayName)
        -- Dashboard uses a completely different frame layout (sidebar navigation),
        -- so we do a full page navigation instead of HTMX content swap
        Lucid.a_ [Lucid.href_ [i|/#{dashboardGetUrl}|], Lucid.class_ $ cls [Tokens.linkText, Tokens.fontBold]] "Dashboard"
        Lucid.a_ [Lucid.href_ [i|/#{userLogoutGetUrl}|], Lucid.class_ "hover:text-gray-800", hxGet_ [i|/#{userLogoutGetUrl}|]] "Logout"

logo :: Lucid.Html ()
logo =
  Lucid.a_ [Lucid.href_ [i|/#{rootGetUrl}|], hxGet_ [i|/#{rootGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, "text-center", "whitespace-pre", "leading-none", "block", "hover:text-gray-600"]] $ do
    Lucid.pre_ [Lucid.style_ "margin: 0;"] $ do
      "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
      "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
      "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
      "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
      "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"

navigation :: Lucid.Html ()
navigation =
  Lucid.nav_ [Lucid.class_ $ cls ["flex", Tokens.gap8, "items-center", "flex-wrap"]] $ do
    Lucid.a_ [Lucid.id_ "nav-donate", Lucid.href_ [i|/#{donateGetUrl}|], hxGet_ [i|/#{donateGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "Donate"
    -- Lucid.a_ [Lucid.id_ "nav-list", Lucid.href_ "/", Lucid.class_ Tokens.navLink] "Listen"
    Lucid.a_ [Lucid.id_ "nav-schedule", Lucid.href_ [i|/#{scheduleGetUrl}|], hxGet_ [i|/#{scheduleGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "Schedule"
    Lucid.a_ [Lucid.id_ "nav-shows", Lucid.href_ [i|/#{showsGetUrl}|], hxGet_ [i|/#{showsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "Shows"
    Lucid.a_ [Lucid.id_ "nav-blog", Lucid.href_ [i|/#{blogGetUrl}|], hxGet_ [i|/#{blogGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "Blog"
    Lucid.a_ [Lucid.id_ "nav-events", Lucid.href_ [i|/#{eventsGetUrl}|], hxGet_ [i|/#{eventsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "Events"
    -- Lucid.a_ [Lucid.id_ "nav-store", Lucid.href_ "/", Lucid.class_ Tokens.navLink] "Store"
    Lucid.a_ [Lucid.id_ "nav-about", Lucid.href_ [i|/#{aboutGetUrl}|], hxGet_ [i|/#{aboutGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ Tokens.navLink] "About"
    Lucid.a_ [Lucid.id_ "nav-contact", Lucid.href_ "mailto:contact@kpbj.fm", Lucid.class_ Tokens.navLink] "Contact"

-- | Suspension warning banner displayed at the top of every page for suspended users
suspensionBanner :: SuspensionStatus -> Lucid.Html ()
suspensionBanner NotSuspended = mempty
suspensionBanner Suspended =
  Lucid.div_ [Lucid.class_ $ cls ["bg-red-600", Tokens.textWhite, Tokens.px4, "py-3", "text-center"]] $ do
    Lucid.div_ [Lucid.class_ $ cls [Tokens.maxWidth, "mx-auto"]] $ do
      Lucid.p_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textLg]] "Account Suspended"
      Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, "mt-1"]] "Your account was suspended."
      Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, "mt-2"]] $
        Lucid.toHtml @Text "If you believe this is an error, please contact us at contact@kpbj.fm"

template :: Maybe UserMetadata.Model -> Lucid.Html () -> Lucid.Html ()
template mUser main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.title_ "KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
      Lucid.script_ [] playerScript
    Lucid.body_ [Lucid.class_ $ cls ["font-mono", Tokens.textGray800, "min-h-screen", "flex", "flex-col"]] $ do
      -- Suspension warning banner (if suspended)
      suspensionBanner $ maybe UserMetadata.NotSuspended UserMetadata.mSuspensionStatus mUser
      -- Persistent header with navigation
      Lucid.header_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.p4]] $ do
        Lucid.div_ [Lucid.class_ $ cls [Tokens.maxWidth, "mx-auto", "flex", "flex-col", "items-center", Tokens.gap4]] $ do
          logo
          musicPlayer
          navigation
          authWidget mUser

      Lucid.div_ [Lucid.class_ $ cls [Tokens.px4, Tokens.maxWidth, "mx-auto", Tokens.fullWidth]] $
        Lucid.div_ [Lucid.id_ bannerContainerId, Lucid.class_ Tokens.fullWidth] mempty
      Lucid.main_
        [Lucid.class_ $ cls ["flex-grow", Tokens.px4, Tokens.py8, Tokens.maxWidth, "mx-auto", Tokens.fullWidth, "flex", "flex-col", "items-center"], Lucid.id_ "main-content"]
        main
      Lucid.footer_ [Lucid.class_ $ cls [Tokens.px4, Tokens.py8, "mt-auto", "text-center"]] $ do
        Lucid.p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"
      -- Script to display banner from URL params (runs after DOM is ready)
      Lucid.script_ [] bannerFromUrlScript

-- footer_ [class_ "bg-gray-800 text-white px-4 py-8 mt-auto"] $ do
--   div_ [class_ "max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8"] $ do
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Listen"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Live Stream"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Show Schedule"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Episode Archive"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Mobile Apps"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Community"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Get A Show"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Volunteer"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Events"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Blog"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Support"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Donate"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Merch Store"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Become a Sponsor"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Membership"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Connect"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Contact Us"
--         li_ $ a_ [href_ "mailto:contact@kpbj.fm", class_ "text-gray-300 hover:text-white hover:underline"] "Email"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Instagram"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Newsletter"
--   div_ [class_ "text-center mt-8 pt-4 border-t border-gray-600 text-gray-400"] $ do
--     p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

--------------------------------------------------------------------------------

loadFrame :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadFrame = pure . template Nothing

loadFrameWithUser :: (Log.MonadLog m, MonadThrow m) => UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
loadFrameWithUser user = pure . template (Just user)

-- | Load content-only for HTMX responses
loadContentOnly :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadContentOnly = pure
