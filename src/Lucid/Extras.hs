module Lucid.Extras where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid.Base qualified as Lucid

--------------------------------------------------------------------------------
-- HTML5

path_ :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
path_ = Lucid.term "path"

ariaHidden_ :: Text -> Lucid.Attributes
ariaHidden_ = Lucid.makeAttributes "aria-hidden"

ariaLabelledby_ :: Text -> Lucid.Attributes
ariaLabelledby_ = Lucid.makeAttributes "aria-labelledby"

viewBox_ :: Text -> Lucid.Attributes
viewBox_ = Lucid.makeAttributes "viewBox"

version_ :: Text -> Lucid.Attributes
version_ = Lucid.makeAttributes "version"

d_ :: Text -> Lucid.Attributes
d_ = Lucid.makeAttributes "d"

dataViewComponent_ :: Text -> Lucid.Attributes
dataViewComponent_ = Lucid.makeAttributes "data-view-component"

--------------------------------------------------------------------------------
-- AlpineJS

xData_ :: Text -> Lucid.Attributes
xData_ = Lucid.makeAttributes "x-data"

xModel_ :: Text -> Lucid.Attributes
xModel_ = Lucid.makeAttributes "x-model"

xBindHidden_ :: Text -> Lucid.Attributes
xBindHidden_ = Lucid.makeAttributes "x-bind:hidden"

xOnBlur_ :: Text -> Lucid.Attributes
xOnBlur_ = Lucid.makeAttributes "x-on:blur"

xOnInput_ :: Text -> Lucid.Attributes
xOnInput_ = Lucid.makeAttributes "x-on:input"

xOnChange_ :: Text -> Lucid.Attributes
xOnChange_ = Lucid.makeAttributes "x-on:change"

xOnClick_ :: Text -> Lucid.Attributes
xOnClick_ = Lucid.makeAttributes "x-on:click"

xOnSubmit_ :: Text -> Lucid.Attributes
xOnSubmit_ = Lucid.makeAttributes "x-on:submit"

xBindClass_ :: Text -> Lucid.Attributes
xBindClass_ = Lucid.makeAttributes "x-bind:class"

xBindDisabled_ :: Text -> Lucid.Attributes
xBindDisabled_ = Lucid.makeAttributes "x-bind:disabled"

xBindStyle_ :: Text -> Lucid.Attributes
xBindStyle_ = Lucid.makeAttributes "x-bind:style"

xRef_ :: Text -> Lucid.Attributes
xRef_ = Lucid.makeAttributes "x-ref"

xHtml_ :: Text -> Lucid.Attributes
xHtml_ = Lucid.makeAttributes "x-html"

xShow_ :: Text -> Lucid.Attributes
xShow_ = Lucid.makeAttributes "x-show"

xText_ :: Text -> Lucid.Attributes
xText_ = Lucid.makeAttributes "x-text"

xOnClickOutside_ :: Text -> Lucid.Attributes
xOnClickOutside_ = Lucid.makeAttributes "x-on:click.outside"

-- | Listen for HTMX's pushedIntoHistory event (fired after hx-push-url updates history)
xOnHtmxPushedIntoHistory_ :: Text -> Lucid.Attributes
xOnHtmxPushedIntoHistory_ = Lucid.makeAttributes "x-on:htmx:pushed-into-history.window"

xTransition_ :: Lucid.Attributes
xTransition_ = Lucid.makeAttributesRaw "x-transition" ""

dataDropdownToggle_ :: Text -> Lucid.Attributes
dataDropdownToggle_ = Lucid.makeAttributes "data-dropdown-toggle"

dataDropdownOffsetDistance_ :: Text -> Lucid.Attributes
dataDropdownOffsetDistance_ = Lucid.makeAttributes "data-dropdown-offset-distance"

dataDropdownTrigger_ :: Text -> Lucid.Attributes
dataDropdownTrigger_ = Lucid.makeAttributes "data-dropdown-trigger"

-- | Vanilla JS onchange handler
onchange_ :: Text -> Lucid.Attributes
onchange_ = Lucid.makeAttributes "onchange"

--------------------------------------------------------------------------------
-- HTMX

hxGet_ :: Text -> Lucid.Attributes
hxGet_ = Lucid.makeAttributes "hx-get"

hxPatch_ :: Text -> Lucid.Attributes
hxPatch_ = Lucid.makeAttributes "hx-patch"

hxPost_ :: Text -> Lucid.Attributes
hxPost_ = Lucid.makeAttributes "hx-post"

hxDelete_ :: Text -> Lucid.Attributes
hxDelete_ = Lucid.makeAttributes "hx-delete"

hxSwap_ :: Text -> Lucid.Attributes
hxSwap_ = Lucid.makeAttributes "hx-swap"

hxTarget_ :: Text -> Lucid.Attributes
hxTarget_ = Lucid.makeAttributes "hx-target"

hxPushUrl_ :: Text -> Lucid.Attributes
hxPushUrl_ = Lucid.makeAttributes "hx-push-url"

hxInclude_ :: Text -> Lucid.Attributes
hxInclude_ = Lucid.makeAttributes "hx-include"

hxParams_ :: Text -> Lucid.Attributes
hxParams_ = Lucid.makeAttributes "hx-params"

hxSelect_ :: Text -> Lucid.Attributes
hxSelect_ = Lucid.makeAttributes "hx-select"

hxTrigger_ :: Text -> Lucid.Attributes
hxTrigger_ = Lucid.makeAttributes "hx-trigger"

hxConfirm_ :: Text -> Lucid.Attributes
hxConfirm_ = Lucid.makeAttributes "hx-confirm"

hxOn_ :: Text -> Lucid.Attributes
hxOn_ = Lucid.makeAttributesRaw "hx-on"

hxOnAfterRequest_ :: Text -> Lucid.Attributes
hxOnAfterRequest_ = Lucid.makeAttributesRaw "hx-on:afterRequest"

hxFollowDirects_ :: Lucid.Attributes
hxFollowDirects_ = Lucid.makeAttributesRaw "hx-follow-redirects" "true"

-- | HTMX out-of-band swap
hxSwapOob_ :: Text -> Lucid.Attributes
hxSwapOob_ = Lucid.makeAttributes "hx-swap-oob"
