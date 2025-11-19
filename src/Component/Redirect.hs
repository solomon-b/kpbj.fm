{-# LANGUAGE QuasiQuotes #-}

module Component.Redirect
  ( redirectTemplate,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified

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
