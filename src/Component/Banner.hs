module Component.Banner
  ( BannerType (..),
    renderBanner,
    bannerContainerId,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
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
      LucidBase.makeAttributes "hx-swap-oob" "true",
      Lucid.class_ Tokens.fullWidth
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "banner",
          class_ $ base [bgColor, Tokens.border2, borderColor, Tokens.p4, Tokens.mb6, Tokens.fullWidth]
        ]
        $ do
          Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
            Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
              Lucid.span_ [Lucid.class_ Tokens.text2xl] $ Lucid.toHtml icon
              Lucid.div_ $ do
                Lucid.h3_ [class_ $ base [Tokens.fontBold, titleColor]] $ Lucid.toHtml title
                Lucid.p_ [class_ $ base [Tokens.textSm, messageColor]] $ Lucid.toHtml message
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#banner').remove()",
                class_ $ base [dismissColor, Tokens.fontBold, Tokens.textXl]
              ]
              "×"
  where
    (bgColor, borderColor, titleColor, messageColor, dismissColor, icon) = case bannerType of
      Success ->
        ( Tokens.successBg,
          Tokens.successBorder,
          Tokens.successText,
          "text-green-700" :: Text,
          "text-green-600 hover:text-green-800" :: Text,
          "✓" :: Text
        )
      Error ->
        ( Tokens.errorBg,
          Tokens.errorBorder,
          Tokens.errorText,
          "text-red-700",
          "text-red-600 hover:text-red-800",
          "✕"
        )
      Warning ->
        ( Tokens.warningBg,
          Tokens.warningBorder,
          Tokens.warningText,
          "text-yellow-700",
          "text-yellow-600 hover:text-yellow-800",
          "⚠"
        )
      Info ->
        ( Tokens.infoBg,
          Tokens.infoBorder,
          Tokens.infoText,
          "text-blue-700",
          "text-blue-600 hover:text-blue-800",
          "ℹ"
        )
