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
          Tokens.successText,
          Tokens.successText <> " hover:opacity-70",
          "✓" :: Text
        )
      Error ->
        ( Tokens.errorBg,
          Tokens.errorBorder,
          Tokens.errorText,
          Tokens.errorText,
          Tokens.errorText <> " hover:opacity-70",
          "✕"
        )
      Warning ->
        ( Tokens.warningBg,
          Tokens.warningBorder,
          Tokens.warningText,
          Tokens.warningText,
          Tokens.warningText <> " hover:opacity-70",
          "⚠"
        )
      Info ->
        ( Tokens.infoBg,
          Tokens.infoBorder,
          Tokens.infoText,
          Tokens.infoText,
          Tokens.infoText <> " hover:opacity-70",
          "ℹ"
        )
