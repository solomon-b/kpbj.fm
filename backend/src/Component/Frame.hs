{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (aboutGetLink, donateGetLink, rootGetLink, userLoginGetLink, userLogoutGetLink, userRegisterGetLink)
import Control.Monad.Catch (MonadThrow)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_)
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

rootGetUrl :: Link.URI
rootGetUrl = Link.linkURI rootGetLink

aboutGetUrl :: Link.URI
aboutGetUrl = Link.linkURI aboutGetLink

donateGetUrl :: Link.URI
donateGetUrl = Link.linkURI donateGetLink

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI $ userLoginGetLink Nothing Nothing

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI $ userRegisterGetLink Nothing Nothing Nothing

userLogoutGetUrl :: Link.URI
userLogoutGetUrl = Link.linkURI userLogoutGetLink

--------------------------------------------------------------------------------

newtype UserInfo = UserInfo {userDisplayName :: DisplayName}

musicPlayer :: Lucid.Html ()
musicPlayer =
  Lucid.div_ [Lucid.class_ "bg-gray-800 text-white p-4 sticky top-0 z-40 border-b-2 border-black"] $ do
    Lucid.div_ [Lucid.class_ "max-w-6xl mx-auto flex items-center gap-4 md:flex-row flex-col"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
        Lucid.button_ [Lucid.class_ "bg-white text-gray-800 px-4 py-2 font-bold cursor-pointer hover:bg-gray-200"] "▶ LIVE"
      Lucid.div_ [Lucid.class_ "flex-grow text-center"] $ do
        Lucid.h3_ [Lucid.class_ "mb-1 font-bold"] "Now Playing: The Midnight Frequency"
        Lucid.p_ [Lucid.class_ "text-sm text-gray-300"] "Host: DJ Nyx • 95.9 FM Shadow Hills • Live Stream"
      Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
        Lucid.span_ [Lucid.class_ "text-sm"] "Vol:"
        Lucid.input_ [Lucid.type_ "range", Lucid.min_ "0", Lucid.max_ "100", Lucid.value_ "80", Lucid.class_ "w-20"]

template :: Maybe UserInfo -> Lucid.Html () -> Lucid.Html ()
template mUser main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.title_ "KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { theme: { extend: { fontFamily: { mono: ['Courier New', 'monospace'] } } } }" :: Text)
    Lucid.body_ [Lucid.class_ "font-mono bg-gray-50 text-gray-800 min-h-screen flex flex-col"] $ do
      Lucid.header_ [Lucid.class_ "bg-white border-b-2 border-gray-800 p-4"] $ do
        Lucid.div_ [Lucid.class_ "max-w-6xl mx-auto flex flex-col items-center gap-4"] $ do
          Lucid.a_ [Lucid.href_ [i|/#{rootGetUrl}|], Lucid.class_ "text-lg font-bold text-center whitespace-pre leading-none block hover:text-gray-600"] $ do
            Lucid.pre_ [Lucid.style_ "margin: 0;"] $ do
              "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
              "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
              "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
              "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
              "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"
          Lucid.nav_ [Lucid.class_ "flex gap-8 items-center flex-wrap"] $ do
            Lucid.a_ [Lucid.href_ [i|/#{donateGetUrl}|], Lucid.class_ "font-bold uppercase hover:underline"] "Donate"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Listen"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Shows"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Archive"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Blog"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Events"
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Store"
            Lucid.a_ [Lucid.href_ [i|/#{aboutGetUrl}|], Lucid.class_ "font-bold uppercase hover:underline"] "About"
            Lucid.a_ [Lucid.href_ "mailto:contact@kpbj.fm", Lucid.class_ "font-bold uppercase hover:underline"] "Contact"
          Lucid.div_ [Lucid.class_ "flex gap-4 items-center text-sm text-gray-600"] $ do
            -- Lucid.a_ [Lucid.href_ "/", Lucid.class_ "hover:text-gray-800"] "🔍 Search"
            case mUser of
              Nothing -> do
                Lucid.a_ [Lucid.href_ [i|/#{userLoginGetUrl}|], Lucid.class_ "hover:text-gray-800"] "Login"
                Lucid.a_ [Lucid.href_ [i|/#{userRegisterGetUrl}|], Lucid.class_ "hover:text-gray-800"] "Sign Up"
              Just user -> do
                Lucid.span_ [Lucid.class_ "text-gray-400"] "•"
                Lucid.span_ [Lucid.class_ "text-gray-800 font-bold"] ("Welcome, " <> Lucid.toHtml (userDisplayName user))
                Lucid.a_ [Lucid.href_ [i|/#{rootGetUrl}|], Lucid.class_ "text-blue-600 hover:text-blue-800 font-bold"] "Dashboard"
                Lucid.a_ [Lucid.href_ [i|/#{userLogoutGetUrl}|], Lucid.class_ "hover:text-gray-800", hxGet_ [i|/#{userLogoutGetUrl}|]] "Logout"
      musicPlayer
      Lucid.main_ [Lucid.class_ "flex-grow px-4 py-8 max-w-6xl mx-auto w-full flex flex-col items-center"] main
      Lucid.footer_ [Lucid.class_ "px-4 py-8 mt-auto text-center"] $ do
        Lucid.p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

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

loadFrameWithUser :: (Log.MonadLog m, MonadThrow m) => UserInfo -> Lucid.Html () -> m (Lucid.Html ())
loadFrameWithUser user = pure . template (Just user)
