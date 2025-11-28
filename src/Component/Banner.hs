{-# LANGUAGE QuasiQuotes #-}

module Component.Banner
  ( BannerType (..),
    renderBanner,
    bannerContainerId,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Base qualified as LucidBase

--------------------------------------------------------------------------------

-- | The type of notification banner to display.
data BannerType
  = Success
  | Error
  | Warning
  | Info
  deriving stock (Show, Eq)

-- | The ID of the banner container element in the Frame.
bannerContainerId :: Text
bannerContainerId = "banner-container"

-- | Render a dismissible notification banner.
--
-- Uses hx-swap-oob to inject into #banner-container.
renderBanner ::
  -- | The type of banner (determines styling)
  BannerType ->
  -- | Title (required)
  Text ->
  -- | Message (required)
  Text ->
  Lucid.Html ()
renderBanner bannerType title message =
  Lucid.div_
    [ Lucid.id_ bannerContainerId,
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "banner",
          Lucid.class_ [i|#{bgColor} border-2 #{borderColor} p-4 mb-6 w-full|]
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] $ Lucid.toHtml icon
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ [i|font-bold #{titleColor}|]] $ Lucid.toHtml title
                Lucid.p_ [Lucid.class_ [i|text-sm #{messageColor}|]] $ Lucid.toHtml message
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#banner').remove()",
                Lucid.class_ [i|#{dismissColor} font-bold text-xl|]
              ]
              "×"
  where
    (bgColor, borderColor, titleColor, messageColor, dismissColor, icon) = case bannerType of
      Success ->
        ( "bg-green-100" :: Text,
          "border-green-600" :: Text,
          "text-green-800" :: Text,
          "text-green-700" :: Text,
          "text-green-600 hover:text-green-800" :: Text,
          "✓" :: Text
        )
      Error ->
        ( "bg-red-100",
          "border-red-600",
          "text-red-800",
          "text-red-700",
          "text-red-600 hover:text-red-800",
          "✕"
        )
      Warning ->
        ( "bg-yellow-100",
          "border-yellow-600",
          "text-yellow-800",
          "text-yellow-700",
          "text-yellow-600 hover:text-yellow-800",
          "⚠"
        )
      Info ->
        ( "bg-blue-100",
          "border-blue-600",
          "text-blue-800",
          "text-blue-700",
          "text-blue-600 hover:text-blue-800",
          "ℹ"
        )
