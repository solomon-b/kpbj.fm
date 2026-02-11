-- | Alpine.js attribute helpers for Lucid2.
--
-- This module provides type-safe attribute builders for Alpine.js directives.
--
-- = Example Usage
--
-- > import Lucid.Alpine
-- > import Lucid qualified
-- >
-- > myComponent :: Lucid.Html ()
-- > myComponent =
-- >   Lucid.div_
-- >     [ xData_ "{ open: false }",
-- >       xShow_ "open"
-- >     ]
-- >     $ do
-- >       Lucid.button_ [xOnClick_ "open = !open"] "Toggle"
-- >       Lucid.div_ [xShow_ "open"] "Hello!"
module Lucid.Alpine
  ( -- * Core Directives
    xData_,
    xInit_,
    xShow_,
    xIf_,
    xFor_,
    xKey_,
    xText_,
    xHtml_,
    xModel_,
    xRef_,

    -- * Event Handlers
    xOn_,
    xOnClick_,
    xOnSubmit_,
    xOnInput_,
    xOnChange_,
    xOnBlur_,
    xOnClickOutside_,
    xOnMousedown_,
    xOnTouchstart_,
    xOnDragover_,
    xOnDragleave_,
    xOnDrop_,

    -- * Bindings
    xBindClass_,
    xBindStyle_,
    xBindDisabled_,
    xBindHidden_,
    xBindSrc_,
    xBindValue_,

    -- * Transitions
    xTransition_,
    xTransitionEnter_,
    xTransitionEnterStart_,
    xTransitionEnterEnd_,
    xTransitionLeave_,
    xTransitionLeaveStart_,
    xTransitionLeaveEnd_,

    -- * HTMX Events (Alpine listeners)
    xOnHtmxPushedIntoHistory_,

    -- * Data Attributes (for Flowbite etc.)
    dataDropdownToggle_,
    dataDropdownOffsetDistance_,
    dataDropdownTrigger_,

    -- * Vanilla JS
    onchange_,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid.Base qualified as Lucid

--------------------------------------------------------------------------------
-- Core Directives

-- | Alpine.js x-data directive.
--
-- > xData_ "{ count: 0 }"
xData_ :: Text -> Lucid.Attributes
xData_ = Lucid.makeAttributes "x-data"

-- | Alpine.js x-init directive.
--
-- > xInit_ "fetchData()"
xInit_ :: Text -> Lucid.Attributes
xInit_ = Lucid.makeAttributes "x-init"

-- | Alpine.js x-show directive.
--
-- > xShow_ "isVisible"
xShow_ :: Text -> Lucid.Attributes
xShow_ = Lucid.makeAttributes "x-show"

-- | Alpine.js x-if directive (must be on a template element).
--
-- > xIf_ "shouldRender"
xIf_ :: Text -> Lucid.Attributes
xIf_ = Lucid.makeAttributes "x-if"

-- | Alpine.js x-for directive.
--
-- > xFor_ "item in items"
xFor_ :: Text -> Lucid.Attributes
xFor_ = Lucid.makeAttributes "x-for"

-- | Alpine.js :key binding for loops.
--
-- > xKey_ "item.id"
xKey_ :: Text -> Lucid.Attributes
xKey_ = Lucid.makeAttributes ":key"

-- | Alpine.js x-text directive.
--
-- > xText_ "message"
xText_ :: Text -> Lucid.Attributes
xText_ = Lucid.makeAttributes "x-text"

-- | Alpine.js x-html directive.
--
-- > xHtml_ "htmlContent"
xHtml_ :: Text -> Lucid.Attributes
xHtml_ = Lucid.makeAttributes "x-html"

-- | Alpine.js x-model directive.
--
-- > xModel_ "formData.email"
xModel_ :: Text -> Lucid.Attributes
xModel_ = Lucid.makeAttributes "x-model"

-- | Alpine.js x-ref directive.
--
-- > xRef_ "inputField"
xRef_ :: Text -> Lucid.Attributes
xRef_ = Lucid.makeAttributes "x-ref"

--------------------------------------------------------------------------------
-- Event Handlers

-- | Generic x-on handler for custom events.
--
-- > xOn_ "dragover.prevent" "isDragging = true"
xOn_ :: Text -> Text -> Lucid.Attributes
xOn_ event = Lucid.makeAttributes ("x-on:" <> event)

-- | Alpine.js x-on:click handler.
--
-- > xOnClick_ "handleClick()"
xOnClick_ :: Text -> Lucid.Attributes
xOnClick_ = Lucid.makeAttributes "x-on:click"

-- | Alpine.js x-on:submit handler.
--
-- > xOnSubmit_ "validateAndSubmit($event)"
xOnSubmit_ :: Text -> Lucid.Attributes
xOnSubmit_ = Lucid.makeAttributes "x-on:submit"

-- | Alpine.js x-on:input handler.
--
-- > xOnInput_ "validate()"
xOnInput_ :: Text -> Lucid.Attributes
xOnInput_ = Lucid.makeAttributes "x-on:input"

-- | Alpine.js x-on:change handler.
--
-- > xOnChange_ "handleChange($event)"
xOnChange_ :: Text -> Lucid.Attributes
xOnChange_ = Lucid.makeAttributes "x-on:change"

-- | Alpine.js x-on:blur handler.
--
-- > xOnBlur_ "validateField()"
xOnBlur_ :: Text -> Lucid.Attributes
xOnBlur_ = Lucid.makeAttributes "x-on:blur"

-- | Alpine.js x-on:click.outside handler.
--
-- > xOnClickOutside_ "open = false"
xOnClickOutside_ :: Text -> Lucid.Attributes
xOnClickOutside_ = Lucid.makeAttributes "x-on:click.outside"

-- | Alpine.js x-on:dragover.prevent handler.
--
-- > xOnDragover_ "isDragging = true"
xOnDragover_ :: Text -> Lucid.Attributes
xOnDragover_ = Lucid.makeAttributes "x-on:dragover.prevent"

-- | Alpine.js x-on:dragleave.prevent handler.
--
-- > xOnDragleave_ "isDragging = false"
xOnDragleave_ :: Text -> Lucid.Attributes
xOnDragleave_ = Lucid.makeAttributes "x-on:dragleave.prevent"

-- | Alpine.js x-on:drop.prevent handler.
--
-- > xOnDrop_ "handleDrop($event)"
xOnDrop_ :: Text -> Lucid.Attributes
xOnDrop_ = Lucid.makeAttributes "x-on:drop.prevent"

-- | Alpine.js x-on:mousedown.prevent handler.
--
-- > xOnMousedown_ "startDrag($event)"
xOnMousedown_ :: Text -> Lucid.Attributes
xOnMousedown_ = Lucid.makeAttributes "x-on:mousedown.prevent"

-- | Alpine.js x-on:touchstart.prevent handler.
--
-- > xOnTouchstart_ "startDrag($event)"
xOnTouchstart_ :: Text -> Lucid.Attributes
xOnTouchstart_ = Lucid.makeAttributes "x-on:touchstart.prevent"

--------------------------------------------------------------------------------
-- Bindings

-- | Alpine.js x-bind:class directive.
--
-- > xBindClass_ "{ 'active': isActive }"
xBindClass_ :: Text -> Lucid.Attributes
xBindClass_ = Lucid.makeAttributes "x-bind:class"

-- | Alpine.js x-bind:style directive.
--
-- > xBindStyle_ "{ color: textColor }"
xBindStyle_ :: Text -> Lucid.Attributes
xBindStyle_ = Lucid.makeAttributes "x-bind:style"

-- | Alpine.js x-bind:disabled directive.
--
-- > xBindDisabled_ "isSubmitting"
xBindDisabled_ :: Text -> Lucid.Attributes
xBindDisabled_ = Lucid.makeAttributes "x-bind:disabled"

-- | Alpine.js x-bind:hidden directive.
--
-- > xBindHidden_ "!isVisible"
xBindHidden_ :: Text -> Lucid.Attributes
xBindHidden_ = Lucid.makeAttributes "x-bind:hidden"

-- | Alpine.js x-bind:src directive.
--
-- > xBindSrc_ "imageUrl"
xBindSrc_ :: Text -> Lucid.Attributes
xBindSrc_ = Lucid.makeAttributes "x-bind:src"

-- | Alpine.js x-bind:value directive.
--
-- > xBindValue_ "computedValue"
xBindValue_ :: Text -> Lucid.Attributes
xBindValue_ = Lucid.makeAttributes "x-bind:value"

--------------------------------------------------------------------------------
-- Transitions

-- | Alpine.js x-transition directive (bare, no value).
--
-- > xTransition_
xTransition_ :: Lucid.Attributes
xTransition_ = Lucid.makeAttributesRaw "x-transition" ""

-- | Alpine.js x-transition:enter directive.
--
-- > xTransitionEnter_ "transition ease-out duration-300"
xTransitionEnter_ :: Text -> Lucid.Attributes
xTransitionEnter_ = Lucid.makeAttributes "x-transition:enter"

-- | Alpine.js x-transition:enter-start directive.
--
-- > xTransitionEnterStart_ "opacity-0 transform scale-95"
xTransitionEnterStart_ :: Text -> Lucid.Attributes
xTransitionEnterStart_ = Lucid.makeAttributes "x-transition:enter-start"

-- | Alpine.js x-transition:enter-end directive.
--
-- > xTransitionEnterEnd_ "opacity-100 transform scale-100"
xTransitionEnterEnd_ :: Text -> Lucid.Attributes
xTransitionEnterEnd_ = Lucid.makeAttributes "x-transition:enter-end"

-- | Alpine.js x-transition:leave directive.
--
-- > xTransitionLeave_ "transition ease-in duration-200"
xTransitionLeave_ :: Text -> Lucid.Attributes
xTransitionLeave_ = Lucid.makeAttributes "x-transition:leave"

-- | Alpine.js x-transition:leave-start directive.
--
-- > xTransitionLeaveStart_ "opacity-100 transform scale-100"
xTransitionLeaveStart_ :: Text -> Lucid.Attributes
xTransitionLeaveStart_ = Lucid.makeAttributes "x-transition:leave-start"

-- | Alpine.js x-transition:leave-end directive.
--
-- > xTransitionLeaveEnd_ "opacity-0 transform scale-95"
xTransitionLeaveEnd_ :: Text -> Lucid.Attributes
xTransitionLeaveEnd_ = Lucid.makeAttributes "x-transition:leave-end"

--------------------------------------------------------------------------------
-- HTMX Events

-- | Listen for HTMX's pushedIntoHistory event.
--
-- > xOnHtmxPushedIntoHistory_ "handleNavigation()"
xOnHtmxPushedIntoHistory_ :: Text -> Lucid.Attributes
xOnHtmxPushedIntoHistory_ = Lucid.makeAttributes "x-on:htmx:pushed-into-history.window"

--------------------------------------------------------------------------------
-- Data Attributes

-- | Flowbite data-dropdown-toggle attribute.
--
-- > dataDropdownToggle_ "dropdownMenu"
dataDropdownToggle_ :: Text -> Lucid.Attributes
dataDropdownToggle_ = Lucid.makeAttributes "data-dropdown-toggle"

-- | Flowbite data-dropdown-offset-distance attribute.
--
-- > dataDropdownOffsetDistance_ "10"
dataDropdownOffsetDistance_ :: Text -> Lucid.Attributes
dataDropdownOffsetDistance_ = Lucid.makeAttributes "data-dropdown-offset-distance"

-- | Flowbite data-dropdown-trigger attribute.
--
-- > dataDropdownTrigger_ "hover"
dataDropdownTrigger_ :: Text -> Lucid.Attributes
dataDropdownTrigger_ = Lucid.makeAttributes "data-dropdown-trigger"

--------------------------------------------------------------------------------
-- Vanilla JS

-- | Vanilla JS onchange handler.
--
-- > onchange_ "this.form.submit()"
onchange_ :: Text -> Lucid.Attributes
onchange_ = Lucid.makeAttributes "onchange"
