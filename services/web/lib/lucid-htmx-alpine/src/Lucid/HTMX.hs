-- | HTMX attribute helpers for Lucid2.
--
-- This module provides type-safe attribute builders for HTMX directives.
--
-- = Example Usage
--
-- > import Lucid.HTMX
-- > import Lucid qualified
-- >
-- > myButton :: Lucid.Html ()
-- > myButton =
-- >   Lucid.button_
-- >     [ hxPost_ "/api/submit",
-- >       hxTarget_ "#result",
-- >       hxSwap_ "innerHTML"
-- >     ]
-- >     "Submit"
module Lucid.HTMX
  ( -- * HTTP Methods
    hxGet_,
    hxPost_,
    hxPatch_,
    hxDelete_,

    -- * Targeting
    hxTarget_,
    hxSwap_,
    hxSelect_,

    -- * Request Configuration
    hxInclude_,
    hxParams_,
    hxPushUrl_,
    hxTrigger_,

    -- * User Interaction
    hxConfirm_,

    -- * Events
    hxOn_,
    hxOnAfterRequest_,

    -- * Indicators
    hxIndicator_,

    -- * Out-of-Band Updates
    hxSwapOob_,

    -- * Redirects
    hxFollowRedirects_,

    -- * History
    hxHistoryElt_,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid.Base qualified as Lucid

--------------------------------------------------------------------------------
-- HTTP Methods

-- | HTMX hx-get attribute.
--
-- > hxGet_ "/api/items"
hxGet_ :: Text -> Lucid.Attributes
hxGet_ = Lucid.makeAttributes "hx-get"

-- | HTMX hx-post attribute.
--
-- > hxPost_ "/api/submit"
hxPost_ :: Text -> Lucid.Attributes
hxPost_ = Lucid.makeAttributes "hx-post"

-- | HTMX hx-patch attribute.
--
-- > hxPatch_ "/api/update/123"
hxPatch_ :: Text -> Lucid.Attributes
hxPatch_ = Lucid.makeAttributes "hx-patch"

-- | HTMX hx-delete attribute.
--
-- > hxDelete_ "/api/items/123"
hxDelete_ :: Text -> Lucid.Attributes
hxDelete_ = Lucid.makeAttributes "hx-delete"

--------------------------------------------------------------------------------
-- Targeting

-- | HTMX hx-target attribute.
--
-- > hxTarget_ "#main-content"
hxTarget_ :: Text -> Lucid.Attributes
hxTarget_ = Lucid.makeAttributes "hx-target"

-- | HTMX hx-swap attribute.
--
-- > hxSwap_ "innerHTML"
-- > hxSwap_ "outerHTML"
-- > hxSwap_ "beforeend"
hxSwap_ :: Text -> Lucid.Attributes
hxSwap_ = Lucid.makeAttributes "hx-swap"

-- | HTMX hx-select attribute.
--
-- > hxSelect_ ".content"
hxSelect_ :: Text -> Lucid.Attributes
hxSelect_ = Lucid.makeAttributes "hx-select"

--------------------------------------------------------------------------------
-- Request Configuration

-- | HTMX hx-include attribute.
--
-- > hxInclude_ "[name='csrf_token']"
hxInclude_ :: Text -> Lucid.Attributes
hxInclude_ = Lucid.makeAttributes "hx-include"

-- | HTMX hx-params attribute.
--
-- > hxParams_ "not name, email"
hxParams_ :: Text -> Lucid.Attributes
hxParams_ = Lucid.makeAttributes "hx-params"

-- | HTMX hx-push-url attribute.
--
-- > hxPushUrl_ "true"
-- > hxPushUrl_ "/new-url"
hxPushUrl_ :: Text -> Lucid.Attributes
hxPushUrl_ = Lucid.makeAttributes "hx-push-url"

-- | HTMX hx-trigger attribute.
--
-- > hxTrigger_ "click"
-- > hxTrigger_ "revealed"
-- > hxTrigger_ "every 2s"
hxTrigger_ :: Text -> Lucid.Attributes
hxTrigger_ = Lucid.makeAttributes "hx-trigger"

--------------------------------------------------------------------------------
-- User Interaction

-- | HTMX hx-confirm attribute.
--
-- > hxConfirm_ "Are you sure you want to delete this?"
hxConfirm_ :: Text -> Lucid.Attributes
hxConfirm_ = Lucid.makeAttributes "hx-confirm"

--------------------------------------------------------------------------------
-- Events

-- | HTMX hx-on attribute (generic, no value).
--
-- > hxOn_
hxOn_ :: Text -> Lucid.Attributes
hxOn_ = Lucid.makeAttributesRaw "hx-on"

-- | HTMX hx-on:afterRequest attribute (raw attribute syntax).
--
-- > hxOnAfterRequest_ "handleResponse()"
hxOnAfterRequest_ :: Text -> Lucid.Attributes
hxOnAfterRequest_ = Lucid.makeAttributesRaw "hx-on:afterRequest"

--------------------------------------------------------------------------------
-- Indicators

-- | HTMX hx-indicator attribute.
--
-- > hxIndicator_ "#spinner"
hxIndicator_ :: Text -> Lucid.Attributes
hxIndicator_ = Lucid.makeAttributes "hx-indicator"

--------------------------------------------------------------------------------
-- Out-of-Band Updates

-- | HTMX hx-swap-oob attribute.
--
-- > hxSwapOob_ "true"
-- > hxSwapOob_ "innerHTML:#notifications"
hxSwapOob_ :: Text -> Lucid.Attributes
hxSwapOob_ = Lucid.makeAttributes "hx-swap-oob"

--------------------------------------------------------------------------------
-- Redirects

-- | HTMX hx-follow-redirects attribute.
--
-- > hxFollowRedirects_
hxFollowRedirects_ :: Lucid.Attributes
hxFollowRedirects_ = Lucid.makeAttributesRaw "hx-follow-redirects" "true"

--------------------------------------------------------------------------------
-- History

-- | HTMX hx-history-elt attribute.
--
-- Marks this element as the history snapshot target. Only this element's
-- innerHTML will be cached and restored during browser history navigation,
-- preserving sibling elements (like audio players) across back/forward.
--
-- > hxHistoryElt_
hxHistoryElt_ :: Lucid.Attributes
hxHistoryElt_ = Lucid.makeAttributesRaw "hx-history-elt" ""
