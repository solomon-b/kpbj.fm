{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Reusable actions dropdown component for dashboard tables.
--
-- This component provides a standardized way to render action dropdowns
-- with proper XSS protection by using data attributes instead of
-- interpolating user content directly into JavaScript.
module Component.ActionsDropdown
  ( -- * Types
    DropdownAction (..),
    Method (..),
    Swap (..),

    -- * Rendering
    render,

    -- * Smart Constructors
    navigateAction,
    htmxDeleteAction,
    htmxPostAction,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras

--------------------------------------------------------------------------------

-- | An action that can be performed from the dropdown.
data DropdownAction
  = -- | Navigate to a URL
    NavigateAction
      { naValue :: Text,
        naLabel :: Text,
        naUrl :: Text
      }
  | -- | Perform an HTMX request
    HtmxAction
      { haValue :: Text,
        haLabel :: Text,
        haMethod :: Method,
        haUrl :: Text,
        haTarget :: Text,
        haSwap :: Swap,
        haConfirm :: Maybe Text,
        haValues :: [(Text, Text)]
      }

-- | HTTP methods for HTMX actions.
data Method
  = MethodGet
  | MethodPost
  | MethodPut
  | MethodPatch
  | MethodDelete
  deriving stock (Eq)

-- | HTMX swap strategies.
data Swap
  = SwapInnerHTML
  | SwapOuterHTML
  deriving stock (Eq)

--------------------------------------------------------------------------------
-- Smart Constructors

-- | Create a simple navigation action.
navigateAction ::
  -- | Option value (e.g., "edit")
  Text ->
  -- | Display label (e.g., "Edit")
  Text ->
  -- | URL to navigate to
  Text ->
  DropdownAction
navigateAction = NavigateAction

-- | Create an HTMX DELETE action with confirmation.
htmxDeleteAction ::
  -- | Option value (e.g., "delete")
  Text ->
  -- | Display label (e.g., "Delete")
  Text ->
  -- | URL to send DELETE request to
  Text ->
  -- | Target selector (e.g., "#main-content")
  Text ->
  -- | Swap strategy
  Swap ->
  -- | Confirmation message
  Text ->
  DropdownAction
htmxDeleteAction value label url target swap confirmMsg =
  HtmxAction
    { haValue = value,
      haLabel = label,
      haMethod = MethodDelete,
      haUrl = url,
      haTarget = target,
      haSwap = swap,
      haConfirm = Just confirmMsg,
      haValues = []
    }

-- | Create an HTMX POST action with optional confirmation and values.
htmxPostAction ::
  -- | Option value (e.g., "suspend")
  Text ->
  -- | Display label (e.g., "Suspend")
  Text ->
  -- | URL to send POST request to
  Text ->
  -- | Target selector
  Text ->
  -- | Swap strategy
  Swap ->
  -- | Optional confirmation message
  Maybe Text ->
  -- | Additional form values
  [(Text, Text)] ->
  DropdownAction
htmxPostAction value label url target swap confirmMsg values =
  HtmxAction
    { haValue = value,
      haLabel = label,
      haMethod = MethodPost,
      haUrl = url,
      haTarget = target,
      haSwap = swap,
      haConfirm = confirmMsg,
      haValues = values
    }

--------------------------------------------------------------------------------
-- Rendering

-- | Render an actions dropdown.
--
-- All dynamic data is stored in data attributes and read by JavaScript,
-- ensuring proper HTML escaping and preventing XSS vulnerabilities.
render :: [DropdownAction] -> Lucid.Html ()
render actions =
  Lucid.select_
    ( [ class_ $ base ["p-2", "border", "border-gray-400", "text-xs", Tokens.bgWhite],
        xData_ "{}",
        xOnChange_ handlerScript,
        xOnClick_ "event.stopPropagation()"
      ]
        <> concatMap actionToDataAttrs actions
    )
    $ do
      Lucid.option_ [Lucid.value_ ""] "Actions..."
      mapM_ renderOption actions

renderOption :: DropdownAction -> Lucid.Html ()
renderOption action =
  Lucid.option_ [Lucid.value_ value] $ Lucid.toHtml label
  where
    (value, label) = case action of
      NavigateAction {naValue, naLabel} -> (naValue, naLabel)
      HtmxAction {haValue, haLabel} -> (haValue, haLabel)

-- | Convert an action to data attributes.
--
-- Uses a naming convention of data-{value}-{property} to namespace
-- each action's configuration.
actionToDataAttrs :: DropdownAction -> [Lucid.Attributes]
actionToDataAttrs (NavigateAction {naValue, naUrl}) =
  [ Lucid.data_ (naValue <> "-type") "navigate",
    Lucid.data_ (naValue <> "-url") naUrl
  ]
actionToDataAttrs (HtmxAction {haValue, haMethod, haUrl, haTarget, haSwap, haConfirm, haValues}) =
  [ Lucid.data_ (haValue <> "-type") "htmx",
    Lucid.data_ (haValue <> "-method") (methodToText haMethod),
    Lucid.data_ (haValue <> "-url") haUrl,
    Lucid.data_ (haValue <> "-target") haTarget,
    Lucid.data_ (haValue <> "-swap") (swapToText haSwap)
  ]
    <> maybe [] (\c -> [Lucid.data_ (haValue <> "-confirm") c]) haConfirm
    <> [Lucid.data_ (haValue <> "-values") (encodeValues haValues) | not (null haValues)]

methodToText :: Method -> Text
methodToText = \case
  MethodGet -> "GET"
  MethodPost -> "POST"
  MethodPut -> "PUT"
  MethodPatch -> "PATCH"
  MethodDelete -> "DELETE"

swapToText :: Swap -> Text
swapToText = \case
  SwapInnerHTML -> "innerHTML"
  SwapOuterHTML -> "outerHTML"

-- | Encode key-value pairs as a simple format for htmx values.
-- Format: key1:value1,key2:value2
encodeValues :: [(Text, Text)] -> Text
encodeValues = Text.intercalate "," . map (\(k, v) -> k <> ":" <> v)

-- | The JavaScript handler that reads action configuration from data attributes.
--
-- This script:
-- 1. Gets the selected action value
-- 2. Resets the select to empty
-- 3. Reads the action type from data-{value}-type
-- 4. For navigate actions: redirects to the URL
-- 5. For htmx actions: optionally confirms, then makes the HTMX request
handlerScript :: Text
handlerScript =
  [i|
const action = $el.value;
if (!action) return;
$el.value = '';

const getAttr = (suffix) => $el.dataset[action + suffix.charAt(0).toUpperCase() + suffix.slice(1)];
const actionType = getAttr('type');

if (actionType === 'navigate') {
  window.location.href = getAttr('url');
} else if (actionType === 'htmx') {
  const confirmMsg = getAttr('confirm');
  if (confirmMsg && !confirm(confirmMsg)) return;

  const method = getAttr('method');
  const url = getAttr('url');
  const target = getAttr('target');
  const swap = getAttr('swap');
  const valuesStr = getAttr('values');

  const opts = { target: target, swap: swap };
  if (valuesStr) {
    opts.values = Object.fromEntries(valuesStr.split(',').map(p => p.split(':')));
  }

  htmx.ajax(method, url, opts);
}
|]
