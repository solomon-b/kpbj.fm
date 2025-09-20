{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

-- import {-# SOURCE #-} API (adminBlogGetLink)
import App.Auth qualified as Auth
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Log qualified
import Lucid (body_, button_, class_, defer_, div_, doctypehtml_, footer_, head_, hr_, href_, id_, li_, link_, main_, nav_, rel_, script_, span_, src_, title_, ul_)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

template :: Lucid.Html () -> Lucid.Html () -> Lucid.Html ()
template navbar main =
  doctypehtml_ $ do
    head_ $ do
      title_ "HyperNet"
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      script_ [src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      script_ [src_ "//unpkg.com/alpinejs", defer_ "true"] (mempty @Text)
    body_ $ do
      div_ [id_ "shell", class_ "container mx-auto"] $ do
        nav_ [id_ "navbar"] navbar
        main_
          [id_ "main", class_ "flex flex-wrap items-center"]
          main
        footer_ [class_ "bg-white m-4"] $ do
          div_ [class_ "w-full max-w-screen-xl mx-auto p-4 md:py-8"] $ do
            hr_ [class_ "my-6 border-gray-200 sm:mx-auto lg:my-8"]
            span_ [class_ "block text-sm text-gray-500 sm:text-center"] "© 1992 HyperNet™. No Rights Reserved."

--------------------------------------------------------------------------------

loadFrame :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadFrame = pure . template mempty
