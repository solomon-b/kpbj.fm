{-# LANGUAGE QuasiQuotes #-}

-- | Notification banners.
--
-- Every banner comes from 'bannerItem'. A banner gets to the screen in one
-- of three ways:
--
-- * In an HTMX response. 'renderBanner' wraps the banner in an out of band
--   swap that appends it to the container.
-- * In a full page. The frame renders the flash message of the request into
--   'bannerContainer'.
-- * From the browser. @window.showBanner@ copies one of the
--   'bannerTemplates' and appends it to the container.
--
-- Banners stack. The user removes one with its dismiss button, and a page
-- change removes all of them.
module Component.Banner
  ( -- * Types
    BannerType (..),
    bannerTypeName,
    bannerTypeFromName,

    -- * Banners
    bannerItem,
    renderBanner,

    -- * Frame parts
    bannerContainer,
    bannerContainerId,
    bannerTemplates,
    bannerTemplateId,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Data.Foldable (find, traverse_)
import Data.String.Interpolate (i)
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
  deriving stock (Show, Eq, Enum, Bounded)

-- | The name of a banner type in the flash cookie and in @window.showBanner@.
bannerTypeName :: BannerType -> Text
bannerTypeName = \case
  Success -> "success"
  Error -> "error"
  Warning -> "warning"
  Info -> "info"

-- | The banner type with a name. The inverse of 'bannerTypeName'.
bannerTypeFromName :: Text -> Maybe BannerType
bannerTypeFromName name = find ((== name) . bannerTypeName) [minBound .. maxBound]

--------------------------------------------------------------------------------

-- | One dismissible banner.
--
-- The title and the message have data attributes, so the browser can fill
-- them in a copy of a template.
bannerItem ::
  -- | The type of banner (determines styling)
  BannerType ->
  -- | Title
  Text ->
  -- | Message
  Text ->
  Lucid.Html ()
bannerItem bannerType title message =
  Lucid.div_
    [ LucidBase.makeAttributes "data-banner" (bannerTypeName bannerType),
      class_ $ base [bgColor, Tokens.border2, borderColor, Tokens.p4, Tokens.fullWidth]
    ]
    $ do
      Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
          Lucid.span_ [Lucid.class_ Tokens.text2xl] $ Lucid.toHtml icon
          Lucid.div_ $ do
            Lucid.h3_ [LucidBase.makeAttributes "data-banner-title" "", class_ $ base [Tokens.fontBold, textColor]] $ Lucid.toHtml title
            Lucid.p_ [LucidBase.makeAttributes "data-banner-message" "", class_ $ base [Tokens.textSm, textColor]] $ Lucid.toHtml message
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.onclick_ "this.closest('[data-banner]').remove()",
            class_ $ base [textColor, "hover:opacity-70", Tokens.fontBold, Tokens.textXl]
          ]
          "×"
  where
    (bgColor, borderColor, textColor) = colorClasses bannerType
    icon = case bannerType of
      Success -> "✓" :: Text
      Error -> "✕"
      Warning -> "⚠"
      Info -> "ℹ"

-- | The tint, the border color, and the text color of a banner type.
colorClasses :: BannerType -> (Text, Text, Text)
colorClasses = \case
  Success -> (Tokens.successBg, Tokens.successBorder, Tokens.successText)
  Error -> (Tokens.errorBg, Tokens.errorBorder, Tokens.errorText)
  Warning -> (Tokens.warningBg, Tokens.warningBorder, Tokens.warningText)
  Info -> (Tokens.infoBg, Tokens.infoBorder, Tokens.infoText)

-- | A banner for an HTMX response.
--
-- The out of band swap appends the banner to @#banner-container@. HTMX
-- inserts the children of the wrapper, not the wrapper itself.
renderBanner ::
  -- | The type of banner (determines styling)
  BannerType ->
  -- | Title
  Text ->
  -- | Message
  Text ->
  Lucid.Html ()
renderBanner bannerType title message =
  Lucid.div_ [LucidBase.makeAttributes "hx-swap-oob" ("beforeend:#" <> bannerContainerId)] $
    bannerItem bannerType title message

--------------------------------------------------------------------------------

-- | The ID of the banner container element in the frame.
bannerContainerId :: Text
bannerContainerId = "banner-container"

-- | The banner container of a frame, with the banners of a full page.
--
-- The container holds all banner spacing. It takes no space when it is
-- empty. An out of band swap appends to it and does not replace it, so it
-- keeps these classes.
bannerContainer :: Lucid.Html () -> Lucid.Html ()
bannerContainer =
  Lucid.div_
    [ Lucid.id_ bannerContainerId,
      class_ $ base ["flex", "flex-col", Tokens.gap2, Tokens.mt4, Tokens.fullWidth, "empty:hidden"]
    ]

-- | The ID of the template for a banner type.
bannerTemplateId :: BannerType -> Text
bannerTemplateId bannerType = [i|banner-template-#{bannerTypeName bannerType}|]

-- | One hidden template for each banner type. @window.showBanner@ copies
-- them, so the browser holds no banner markup of its own.
--
-- Put them outside 'bannerContainer', or the container is never empty.
bannerTemplates :: Lucid.Html ()
bannerTemplates = traverse_ template [minBound .. maxBound]
  where
    template bannerType =
      Lucid.template_ [Lucid.id_ (bannerTemplateId bannerType)] $
        bannerItem bannerType "" ""

--------------------------------------------------------------------------------
-- Tailwind

-- | The color classes of every banner type. "Component.Tailwind" puts them in
-- the safelist, because a banner can first appear in an HTMX swap.
safelist :: [Text]
safelist = foldMap typeClasses [minBound .. maxBound]
  where
    typeClasses bannerType =
      let (bgColor, borderColor, textColor) = colorClasses bannerType
       in [bgColor, borderColor, textColor]
