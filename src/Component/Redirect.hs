{-# LANGUAGE QuasiQuotes #-}

module Component.Redirect
  ( redirectTemplate,
    redirectWithBanner,
    BannerParams (..),
  )
where

--------------------------------------------------------------------------------

import Component.Banner (BannerType (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Lucid qualified
import Network.HTTP.Types.URI (urlEncode)

--------------------------------------------------------------------------------

-- | Minimal redirect template
--
-- For HTMX requests, the HX-Redirect header will handle the redirect.
-- For regular browser requests, this uses a meta refresh tag.
-- Works even with JavaScript disabled.
redirectTemplate :: Text -> Lucid.Html ()
redirectTemplate url = do
  Lucid.meta_ [Lucid.httpEquiv_ "refresh", Lucid.content_ [i|0;url=#{url}|]]
  Lucid.p_ "Redirecting..."

-- | Parameters for a banner to display after redirect
data BannerParams = BannerParams
  { bpType :: BannerType,
    bpTitle :: Text,
    bpMessage :: Text
  }

-- | Redirect with banner parameters encoded in the URL
--
-- Appends query parameters that Component.Frame will read on page load
-- to display a notification banner.
redirectWithBanner :: Text -> BannerParams -> Lucid.Html ()
redirectWithBanner baseUrl BannerParams {..} = do
  let bannerTypeParam :: Text
      bannerTypeParam = case bpType of
        Success -> "success"
        Error -> "error"
        Warning -> "warning"
        Info -> "info"
      encodedTitle = urlEncodeText bpTitle
      encodedMessage = urlEncodeText bpMessage
      separator :: Text
      separator = if Text.isInfixOf "?" baseUrl then "&" else "?"
      fullUrl :: Text
      fullUrl = [i|#{baseUrl}#{separator}_banner=#{bannerTypeParam}&_title=#{encodedTitle}&_msg=#{encodedMessage}|]
  Lucid.meta_ [Lucid.httpEquiv_ "refresh", Lucid.content_ [i|0;url=#{fullUrl}|]]
  Lucid.p_ "Redirecting..."

-- | URL-encode a Text value
urlEncodeText :: Text -> Text
urlEncodeText = Text.Encoding.decodeUtf8 . urlEncode True . Text.Encoding.encodeUtf8
