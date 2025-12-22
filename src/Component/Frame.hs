{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

import API.Links (apiLinks, blogLinks, dashboardLinks, eventsLinks, scheduleLink, showsLinks, userLinks)
import API.Types
import Component.Banner (bannerContainerId)
import Control.Monad.Catch (MonadThrow)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.DisplayName (DisplayName)
import Effects.Database.Tables.UserMetadata (SuspensionStatus (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xInit_, xModel_, xOnClickOutside_, xOnClick_, xOnInput_, xRef_, xShow_, xText_, xTransition_)
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
eventsGetUrl = Link.linkURI eventsLinks.list

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
showsGetUrl = Link.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

newtype UserInfo = UserInfo {userDisplayName :: DisplayName}

-- | Desktop music player (hidden on mobile)
-- Contains the shared audio element and Alpine state
desktopMusicPlayer :: Lucid.Html ()
desktopMusicPlayer =
  Lucid.div_
    [ class_ $ do
        base [Tokens.px6, "text-center"]
        tablet ["block"]
    ]
    $ do
      Lucid.div_ [class_ $ base ["inline-flex", "items-center", Tokens.gap4, Tokens.textSm, "font-mono"]] $ do
        Lucid.button_
          [ Lucid.class_ "hover:text-gray-600 dark:hover:text-gray-400 cursor-pointer bg-transparent border-none",
            xOnClick_ "toggle()",
            xText_ "isPlaying ? '[ PAUSE ]' : '[ PLAY ]'"
          ]
          "[ PLAY ]"
        Lucid.span_ [Lucid.class_ "text-gray-500 dark:text-gray-500"] "|"
        -- Now playing info
        Lucid.div_ [xText_ "currentShow || 'NOW PLAYING: KPBJ 95.9 FM'"] "NOW PLAYING: KPBJ 95.9 FM"
        -- Back to Live button (only visible when in episode mode)
        Lucid.span_ [xShow_ "mode === 'episode'", Lucid.class_ "text-gray-500 dark:text-gray-500"] "|"
        Lucid.button_
          [ xShow_ "mode === 'episode'",
            xOnClick_ "playStream()",
            Lucid.class_ "hover:text-gray-600 dark:hover:text-gray-400 text-xs uppercase cursor-pointer bg-transparent border-none"
          ]
          "[ BACK TO LIVE ]"
        Lucid.span_ [Lucid.class_ "text-gray-500 dark:text-gray-500"] "|"
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
          Lucid.span_ "VOL:"
          Lucid.input_
            [ Lucid.type_ "range",
              Lucid.min_ "0",
              Lucid.max_ "100",
              xModel_ "volume",
              xOnInput_ "setVolume($event.target.value)",
              Lucid.class_ "w-20 h-1"
            ]

-- | Fixed mobile player at bottom of viewport
-- Minimal UI: play/pause + show name only
mobileMusicPlayer :: Lucid.Html ()
mobileMusicPlayer =
  Lucid.div_
    [ class_ $ do
        base ["flex", "items-center", "justify-around", Tokens.gap4, Tokens.textSm, "font-mono", Tokens.textWhite]
    ]
    $ do
      -- Play/pause button (using button element to avoid page jump from href="#")
      Lucid.button_
        [ xOnClick_ "toggle()",
          class_ $ base ["hover:text-gray-300", "cursor-pointer", "text-center", "flex-shrink-0", "whitespace-nowrap", "bg-transparent", "border-none"]
        ]
        $ do
          -- Both rendered, visibility toggled - prevents layout shift
          Lucid.span_ [xShow_ "!isPlaying"] "[ PLAY ]"
          Lucid.span_ [xShow_ "isPlaying"] "[ PAUSE ]"
      -- Show name (scrolling marquee only when text overflows)
      Lucid.div_
        [ xData_ "{ needsScroll: false }",
          xInit_ "const observer = new ResizeObserver(() => { needsScroll = $refs.text && $refs.container ? $refs.text.scrollWidth > $refs.container.clientWidth : false; }); if ($refs.text) observer.observe($refs.text);",
          xRef_ "container",
          class_ $ base ["flex-1", "min-w-0", "text-center", "uppercase", "marquee-container"]
        ]
        $ do
          Lucid.div_ [Lucid.class_ "marquee-track", xBindClass_ "{ 'scrolling': needsScroll }"] $ do
            -- First copy of text
            Lucid.span_
              [ xRef_ "text",
                xText_ "currentShow || 'KPBJ 95.9 FM'",
                class_ $ base [Tokens.fontBold, "marquee-text"]
              ]
              "KPBJ 95.9 FM"
            -- Second copy (only visible when scrolling)
            Lucid.span_
              [ xShow_ "needsScroll",
                xText_ "currentShow || 'KPBJ 95.9 FM'",
                class_ $ base [Tokens.fontBold, "marquee-text"]
              ]
              "KPBJ 95.9 FM"

-- | Banner above mobile player when playing archived episode (entire banner is clickable)
mobileArchiveBanner :: Lucid.Html ()
mobileArchiveBanner =
  Lucid.button_
    [ xShow_ "mode === 'episode'",
      xOnClick_ "playStream()",
      class_ $ do
        base ["flex", "items-center", "justify-center", Tokens.gap4, "bg-gray-600", Tokens.textWhite, Tokens.px4, "py-2", Tokens.textXs, "font-mono", "w-full", "border-none", "cursor-pointer", "hover:bg-gray-500"]
    ]
    $ do
      Lucid.span_ "Playing archived episode"
      Lucid.span_ [class_ $ base ["bg-white", "text-gray-800", "px-3", "py-1", Tokens.fontBold]] "GO LIVE"

-- | Audio element wrapper with Alpine state (shared between mobile and desktop)
musicPlayerWrapper :: Lucid.Html () -> Lucid.Html ()
musicPlayerWrapper content =
  Lucid.div_ [xData_ playerData, class_ $ base ["flex", "flex-col", "flex-1"]] $ do
    Lucid.audio_
      [ xRef_ "audio",
        Lucid.preload_ "none"
      ]
      mempty
    content
  where
    playerData =
      [i|{
      playerId: 'navbar-player',
      isPlaying: false,
      volume: 80,
      streamUrl: 'https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3',
      metadataUrl: 'https://kpbj.hasnoskills.com/listen/kpbj_test_station/status-json.xsl',

      // Playback mode: 'stream' for live radio, 'episode' for on-demand episodes
      mode: 'stream',
      episodeUrl: '',
      episodeTitle: '',

      // Metadata (for live stream)
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

        // Use current mode's source
        const sourceUrl = this.mode === 'episode' ? this.episodeUrl : this.streamUrl;

        // Load source if needed
        if (!audio.src || audio.src === '') {
          audio.src = sourceUrl;
        }

        audio.volume = this.volume / 100;

        audio.play()
          .then(() => {
            console.log('Navbar player: Playing in ' + this.mode + ' mode');
            this.isPlaying = true;
            // Only poll metadata for live stream
            if (this.mode === 'stream') {
              this.startMetadataPolling();
            }
          })
          .catch((error) => {
            console.error('Navbar player failed:', error);
            this.errorMessage = 'Failed to start: ' + error.message;
            setTimeout(() => this.errorMessage = '', 5000);
          });
      },

      pause() {
        console.log('Navbar player: Pausing');
        const audio = this.$refs.audio;
        audio.pause();
        this.isPlaying = false;
        this.stopMetadataPolling();
      },

      // Play an episode (called from episode cards via playInNavbar global function)
      playEpisode(url, title) {
        console.log('Navbar player: Switching to episode mode', title);

        // Stop current playback
        this.pause();

        // Switch to episode mode
        this.mode = 'episode';
        this.episodeUrl = url;
        this.episodeTitle = title;
        this.currentShow = title;

        // Clear and reload audio source
        const audio = this.$refs.audio;
        audio.src = url;

        // Start playing
        this.play();
      },

      // Return to livestream playback
      playStream() {
        console.log('Navbar player: Switching to stream mode');

        // Stop current playback
        this.pause();

        // Switch to stream mode
        this.mode = 'stream';
        this.episodeUrl = '';
        this.episodeTitle = '';
        this.currentShow = '';

        // Clear and reload audio source
        const audio = this.$refs.audio;
        audio.src = this.streamUrl;

        // Start playing and fetch metadata
        this.fetchMetadata();
        this.play();
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

    // Play an episode in the navbar player (called by episode cards)
    function playInNavbar(episodeAudioUrl, episodeTitle) {
      // Find navbar player component
      const navbarPlayerEl = document.querySelector('[x-data*="navbar-player"]');
      if (navbarPlayerEl) {
        const navbarPlayer = Alpine.$data(navbarPlayerEl);
        if (navbarPlayer && typeof navbarPlayer.playEpisode === 'function') {
          navbarPlayer.playEpisode(episodeAudioUrl, episodeTitle);
        }
      }
    }

    // Check if navbar is currently playing a specific episode URL
    function isNavbarPlayingEpisode(episodeAudioUrl) {
      const navbarPlayerEl = document.querySelector('[x-data*="navbar-player"]');
      if (navbarPlayerEl) {
        const navbarPlayer = Alpine.$data(navbarPlayerEl);
        return navbarPlayer &&
               navbarPlayer.isPlaying &&
               navbarPlayer.mode === 'episode' &&
               navbarPlayer.episodeUrl === episodeAudioUrl;
      }
      return false;
    }

    // Toggle episode playback in navbar (pause if playing this episode, play if not)
    function toggleEpisodeInNavbar(episodeAudioUrl, episodeTitle) {
      const navbarPlayerEl = document.querySelector('[x-data*="navbar-player"]');
      if (navbarPlayerEl) {
        const navbarPlayer = Alpine.$data(navbarPlayerEl);
        if (navbarPlayer) {
          if (isNavbarPlayingEpisode(episodeAudioUrl)) {
            navbarPlayer.pause();
          } else {
            navbarPlayer.playEpisode(episodeAudioUrl, episodeTitle);
          }
        }
      }
    }
  |]

-- | Dark mode script that applies theme based on user's color scheme setting
-- ColorScheme values: "Automatic" (follow system), "LightMode", "DarkMode"
darkModeScript :: Maybe UserMetadata.ColorScheme -> Text
darkModeScript mColorScheme =
  let colorSchemeJs = case mColorScheme of
        Nothing -> "Automatic" :: Text
        Just UserMetadata.Automatic -> "Automatic"
        Just UserMetadata.LightMode -> "LightMode"
        Just UserMetadata.DarkMode -> "DarkMode"
   in [i|
    (function() {
      const colorScheme = '#{colorSchemeJs}';

      function applyTheme() {
        if (colorScheme === 'DarkMode') {
          document.documentElement.classList.add('dark');
        } else if (colorScheme === 'LightMode') {
          document.documentElement.classList.remove('dark');
        } else {
          // Automatic - follow system preference
          if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
            document.documentElement.classList.add('dark');
          } else {
            document.documentElement.classList.remove('dark');
          }
        }
      }

      // Apply theme immediately
      applyTheme();

      // Listen for system preference changes (only matters for Automatic mode)
      if (colorScheme === 'Automatic') {
        window.matchMedia('(prefers-color-scheme: dark)')
          .addEventListener('change', applyTheme);
      }
    })();
  |]

-- | CSS for marquee scrolling text animation (only scrolls when text overflows)
marqueeStyles :: Text
marqueeStyles =
  [i|
    @keyframes marquee {
      0% { transform: translateX(0); }
      100% { transform: translateX(-50%); }
    }
    .marquee-container {
      overflow: hidden;
      white-space: nowrap;
    }
    .marquee-track {
      display: inline-block;
    }
    .marquee-track.scrolling {
      animation: marquee 10s linear infinite;
    }
    .marquee-text {
      display: inline-block;
      padding-right: 2rem;
    }
  |]

-- | JavaScript to highlight the active navigation link based on current URL
activeNavScript :: Text
activeNavScript =
  [i|
    function updateActiveNav() {
      const path = window.location.pathname;
      document.querySelectorAll('nav a[id^="nav-"]').forEach(el => {
        const href = el.getAttribute('href');
        const isActive = path === href || path.startsWith(href + '/');
        el.classList.toggle('underline', isActive);
      });
    }

    document.addEventListener('DOMContentLoaded', updateActiveNav);
    document.addEventListener('htmx:pushedIntoHistory', updateActiveNav);
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
  Lucid.div_ [class_ $ base ["flex", Tokens.gap4, "items-center", Tokens.textSm, Tokens.textGray600, "dark:text-gray-400"]] $ do
    case mUser of
      Nothing -> do
        Lucid.a_ [Lucid.href_ [i|/#{userLoginGetUrl}|], hxGet_ [i|/#{userLoginGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800 dark:hover:text-gray-200"] "Login"
        Lucid.a_ [Lucid.href_ [i|/#{userRegisterGetUrl}|], hxGet_ [i|/#{userRegisterGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800 dark:hover:text-gray-200"] "Sign Up"
      Just user -> do
        Lucid.span_ [Lucid.class_ "text-gray-400 dark:text-gray-500"] "•"
        Lucid.span_ [class_ $ base [Tokens.textGray800, "dark:text-gray-200", Tokens.fontBold]] ("Welcome, " <> Lucid.toHtml user.mDisplayName)
        -- Dashboard uses a completely different frame layout (sidebar navigation),
        -- so we do a full page navigation instead of HTMX content swap
        Lucid.a_ [Lucid.href_ [i|/#{dashboardGetUrl}|], class_ $ base [Tokens.linkText, "dark:text-blue-400", Tokens.fontBold]] "Dashboard"
        Lucid.a_ [Lucid.href_ [i|/#{userLogoutGetUrl}|], Lucid.class_ "hover:text-gray-800 dark:hover:text-gray-200", hxGet_ [i|/#{userLogoutGetUrl}|]] "Logout"

logo :: Lucid.Html ()
logo =
  Lucid.a_ [Lucid.href_ [i|/#{rootGetUrl}|], hxGet_ [i|/#{rootGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", class_ $ base [Tokens.textLg, Tokens.fontBold, "text-center", "whitespace-pre", "leading-none", "block", "hover:text-gray-600", "dark:hover:text-gray-400"]] $ do
    Lucid.pre_ [Lucid.style_ "margin: 0;"] $ do
      "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
      "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
      "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
      "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
      "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"

-- | Compact logo for mobile header
miniLogo :: Lucid.Html ()
miniLogo =
  Lucid.a_
    [ Lucid.href_ [i|/#{rootGetUrl}|],
      hxGet_ [i|/#{rootGetUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      xOnClick_ "menuOpen = false",
      class_ $ base [Tokens.fontBold, "text-center", "whitespace-pre", "leading-none", "hover:text-gray-600", "dark:hover:text-gray-400"]
    ]
    $ do
      Lucid.pre_ [Lucid.style_ "margin: 0; font-size: 0.5rem;"] $ do
        "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
        "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
        "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
        "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
        "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"

--------------------------------------------------------------------------------
-- Mobile Components

-- | Mobile header bar with hamburger menu and centered logo
-- Only visible on mobile (hidden on md+)
mobileHeader :: Maybe UserMetadata.Model -> Lucid.Html ()
mobileHeader _mUser =
  Lucid.div_
    [ class_ $ do
        base ["flex", "items-center", Tokens.fullWidth, Tokens.px4, "py-3", Tokens.bgWhite, "dark:bg-gray-800"]
        tablet ["hidden"]
    ]
    $ do
      -- Hamburger button
      Lucid.button_
        [ xOnClick_ "menuOpen = !menuOpen",
          class_ $ base [Tokens.p2, "hover:bg-gray-100", "dark:hover:bg-gray-700", Tokens.fontBold, Tokens.textLg]
        ]
        "☰"
      -- Centered logo (flex-1 pushes it to center)
      Lucid.div_ [class_ $ base ["flex-1", "flex", "justify-center"]] miniLogo
      -- Invisible spacer to balance hamburger button (same size)
      Lucid.div_ [class_ $ base [Tokens.p2, Tokens.textLg, "invisible"]] $ Lucid.toHtml @Text "☰"

-- | Simplified auth widget for mobile header
mobileAuthWidget :: Maybe UserMetadata.Model -> Lucid.Html ()
mobileAuthWidget mUser =
  case mUser of
    Nothing ->
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.px3, Tokens.py2, Tokens.cardBorder, "dark:border-gray-600", Tokens.fontBold, Tokens.textSm, "hover:bg-gray-100", "dark:hover:bg-gray-700"]
        ]
        "Log in"
    Just _user ->
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardGetUrl}|],
          class_ $ base [Tokens.px3, Tokens.py2, Tokens.cardBorder, "dark:border-gray-600", Tokens.fontBold, Tokens.textSm, "hover:bg-gray-100", "dark:hover:bg-gray-700"]
        ]
        "Dashboard"

-- | Full-screen mobile menu overlay
-- Appears when hamburger is tapped, smooth fade/slide transition
mobileMenuOverlay :: Maybe UserMetadata.Model -> Lucid.Html ()
mobileMenuOverlay mUser =
  Lucid.div_
    [ xShow_ "menuOpen",
      xOnClickOutside_ "menuOpen = false",
      xTransition_,
      class_ $ base ["fixed", "inset-0", "z-50", Tokens.bgWhite, "dark:bg-gray-900", "flex", "flex-col", Tokens.p6]
    ]
    $ do
      -- Close button row
      Lucid.div_ [class_ $ base ["flex", "justify-end", "items-center", Tokens.mb8]] $ do
        Lucid.button_
          [ xOnClick_ "menuOpen = false",
            class_ $ base [Tokens.p2, "hover:bg-gray-100", "dark:hover:bg-gray-700", Tokens.fontBold, "text-2xl", "dark:text-gray-200"]
          ]
          "×"
      -- Navigation links
      mobileNavLinks mUser

-- | Navigation links for mobile menu (vertical list)
mobileNavLinks :: Maybe UserMetadata.Model -> Lucid.Html ()
mobileNavLinks mUser =
  Lucid.nav_ [class_ $ base ["flex", "flex-col", Tokens.gap6]] $ do
    mobileNavLink "Home" rootGetUrl
    mobileNavLink "Shows" showsGetUrl
    mobileNavLink "Schedule" scheduleGetUrl
    mobileNavLink "Donate" donateGetUrl
    mobileNavLink "Events" eventsGetUrl
    mobileNavLink "Blog" blogGetUrl
    mobileNavLink "About" aboutGetUrl
    Lucid.a_
      [ Lucid.href_ "mailto:contact@kpbj.fm",
        xOnClick_ "menuOpen = false",
        class_ $ base [Tokens.textXl, Tokens.fontBold, "hover:text-gray-600", "dark:text-gray-200", "dark:hover:text-gray-400"]
      ]
      "Contact"
    -- Divider
    Lucid.hr_ [class_ $ base ["border-gray-300", "dark:border-gray-600", "my-2"]]
    -- Auth links
    case mUser of
      Nothing -> do
        mobileNavLink "Log In" userLoginGetUrl
        mobileNavLink "Sign Up" userRegisterGetUrl
      Just _user -> do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardGetUrl}|],
            xOnClick_ "menuOpen = false",
            class_ $ base [Tokens.textXl, Tokens.fontBold, "hover:text-gray-600", "dark:text-gray-200", "dark:hover:text-gray-400"]
          ]
          "Dashboard"
        Lucid.a_
          [ Lucid.href_ [i|/#{userLogoutGetUrl}|],
            hxGet_ [i|/#{userLogoutGetUrl}|],
            xOnClick_ "menuOpen = false",
            class_ $ base [Tokens.textXl, Tokens.fontBold, "hover:text-gray-600", "dark:text-gray-200", "dark:hover:text-gray-400"]
          ]
          "Log Out"

-- | Single navigation link for mobile menu
mobileNavLink :: Lucid.Html () -> Link.URI -> Lucid.Html ()
mobileNavLink label url =
  Lucid.a_
    [ Lucid.href_ [i|/#{url}|],
      hxGet_ [i|/#{url}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      xOnClick_ "menuOpen = false",
      class_ $ base [Tokens.textXl, Tokens.fontBold, "hover:text-gray-600", "dark:text-gray-200", "dark:hover:text-gray-400"]
    ]
    label

-- | Navigation link classes with dark mode support
navLinkDark :: Text
navLinkDark = Tokens.navLink <> " dark:text-gray-300 dark:hover:text-white"

navigation :: Lucid.Html ()
navigation =
  Lucid.nav_ [class_ $ base ["flex", Tokens.gap8, "items-center", "flex-wrap"]] $ do
    Lucid.a_ [Lucid.id_ "nav-shows", Lucid.href_ [i|/#{showsGetUrl}|], hxGet_ [i|/#{showsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "Shows"
    Lucid.a_ [Lucid.id_ "nav-schedule", Lucid.href_ [i|/#{scheduleGetUrl}|], hxGet_ [i|/#{scheduleGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "Schedule"
    Lucid.a_ [Lucid.id_ "nav-donate", Lucid.href_ [i|/#{donateGetUrl}|], hxGet_ [i|/#{donateGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "Donate"
    Lucid.a_ [Lucid.id_ "nav-events", Lucid.href_ [i|/#{eventsGetUrl}|], hxGet_ [i|/#{eventsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "Events"
    Lucid.a_ [Lucid.id_ "nav-blog", Lucid.href_ [i|/#{blogGetUrl}|], hxGet_ [i|/#{blogGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "Blog"
    Lucid.a_ [Lucid.id_ "nav-about", Lucid.href_ [i|/#{aboutGetUrl}|], hxGet_ [i|/#{aboutGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ navLinkDark] "About"
    Lucid.a_ [Lucid.id_ "nav-contact", Lucid.href_ "mailto:contact@kpbj.fm", Lucid.class_ navLinkDark] "Contact"

-- | Suspension warning banner displayed at the top of every page for suspended users
suspensionBanner :: SuspensionStatus -> Lucid.Html ()
suspensionBanner NotSuspended = mempty
suspensionBanner Suspended =
  Lucid.div_ [class_ $ base ["bg-red-600", Tokens.textWhite, Tokens.px4, "py-3", "text-center"]] $ do
    Lucid.div_ [class_ $ base [Tokens.maxWidth, "mx-auto"]] $ do
      Lucid.p_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] "Account Suspended"
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-1"]] "Your account was suspended."
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] $
        Lucid.toHtml @Text "If you believe this is an error, please contact us at contact@kpbj.fm"

template :: Maybe UserMetadata.Model -> Lucid.Html () -> Lucid.Html ()
template mUser main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "width=device-width, initial-scale=1.0"]
      Lucid.title_ "KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { darkMode: 'class', theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
      Lucid.script_ [] playerScript
      Lucid.script_ [] (darkModeScript (UserMetadata.mColorScheme <$> mUser))
      Lucid.script_ [] activeNavScript
      Lucid.style_ [] marqueeStyles
    Lucid.body_
      [ class_ $ do
          base ["font-mono", Tokens.textGray800, "dark:text-gray-200", "dark:bg-gray-900", "min-h-screen", "flex", "flex-col", "pb-20"]
          tablet ["pb-0"]
      ]
      $ do
        -- Suspension warning banner (if suspended)
        suspensionBanner $ maybe UserMetadata.NotSuspended UserMetadata.mSuspensionStatus mUser

        -- Wrap entire page in Alpine component for shared player + menu state
        Lucid.div_ [xData_ "{ menuOpen: false }", class_ $ base ["flex", "flex-col", "flex-1"]] $ do
          -- Mobile menu overlay (full-screen, z-50)
          mobileMenuOverlay mUser

          -- Player wrapper with Alpine state (audio element shared between desktop/mobile)
          musicPlayerWrapper $ do
            -- Mobile header (hamburger + mini logo + login) - visible only on mobile
            mobileHeader mUser

            -- Desktop header (hidden on mobile)
            Lucid.header_
              [ class_ $ do
                  base ["hidden", Tokens.bgWhite, "dark:bg-gray-800", Tokens.p4]
                  tablet ["block"]
              ]
              $ do
                Lucid.div_ [class_ $ base [Tokens.maxWidth, "mx-auto", "flex", "flex-col", "items-center", Tokens.gap4]] $ do
                  logo
                  desktopMusicPlayer
                  navigation
                  authWidget mUser

            -- Banner container
            Lucid.div_ [class_ $ base [Tokens.px4, Tokens.maxWidth, "mx-auto", Tokens.fullWidth]] $
              Lucid.div_ [Lucid.id_ bannerContainerId, Lucid.class_ Tokens.fullWidth] mempty

            -- Main content
            Lucid.main_
              [ class_ $ do
                  base ["flex-grow", Tokens.px4, Tokens.py4, Tokens.maxWidth, "mx-auto", Tokens.fullWidth, "flex", "flex-col"]
                  tablet [Tokens.px4, "items-center"],
                Lucid.id_ "main-content"
              ]
              main

            -- Footer
            Lucid.footer_ [class_ $ base [Tokens.px4, Tokens.py2, "mt-auto", "text-center", Tokens.textXs, Tokens.textGray600, "dark:text-gray-400"]] $ do
              Lucid.p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

            -- Fixed mobile player at bottom (visible only on mobile)
            Lucid.div_
              [ class_ $ do
                  base ["fixed", "bottom-0", "left-0", "right-0", "z-40"]
                  tablet ["hidden"]
              ]
              $ do
                -- Banner above player when playing archived episode
                mobileArchiveBanner
                -- Main player bar
                Lucid.div_ [class_ $ base [Tokens.bgGray800, Tokens.px4, "py-3"]] mobileMusicPlayer

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
