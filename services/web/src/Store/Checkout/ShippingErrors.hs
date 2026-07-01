-- | Human-readable messages for EasyPost shipping failures.
--
-- Turns the two ways EasyPost signals a bad address into user-facing copy:
--
--   * A failed API call (non-2xx) whose body carries a structured
--     @{"error": {...}}@ payload — see 'easyPostFailureMessage'.
--   * A successful shipment whose to-address delivery verification came back
--     unsuccessful — see 'deliveryVerificationError'.
--
-- Both paths fall back to a safe generic message and never return empty text.
module Store.Checkout.ShippingErrors
  ( easyPostFailureMessage,
    deliveryVerificationError,
  )
where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import EasyPost.Client (EasyPostClientError, easyPostErrorDetails)
import EasyPost.Types
  ( EasyPostError (..),
    EasyPostFieldError (..),
    Shipment (..),
    Verification (..),
  )

--------------------------------------------------------------------------------

-- | Safe fallback used whenever we can't produce anything more specific.
genericFallback :: Text
genericFallback = "We couldn't process your shipping address. Please check it and try again."

-- | Build a user-facing message from an EasyPost client error.
--
-- Prefers the structured field errors, then the top-level message, then a
-- generic fallback. Always returns non-empty text.
easyPostFailureMessage :: EasyPostClientError -> Text
easyPostFailureMessage err =
  maybe genericFallback formatEasyPostError (easyPostErrorDetails err)

-- | Format a decoded 'EasyPostError' into user-facing copy.
formatEasyPostError :: EasyPostError -> Text
formatEasyPostError easyPostError =
  case formatFieldErrors easyPostError.errors of
    Just msg -> msg
    Nothing ->
      if Text.null (Text.strip easyPostError.message)
        then genericFallback
        else easyPostError.message

-- | Produce a message from a delivery verification, when it failed.
--
-- Returns 'Nothing' when the shipment has no verification block or the
-- delivery verification succeeded.
deliveryVerificationError :: Shipment -> Maybe Text
deliveryVerificationError shipment =
  case shipment.toAddressDeliveryVerification of
    Just verification
      | not verification.success ->
          Just (fromMaybe genericFallback (formatFieldErrors verification.errors))
    _ -> Nothing

--------------------------------------------------------------------------------

-- | Join the field errors into a single sentence, or 'Nothing' if there are
-- none with usable text.
formatFieldErrors :: [EasyPostFieldError] -> Maybe Text
formatFieldErrors fieldErrors =
  case filter (not . Text.null) (map formatFieldError fieldErrors) of
    [] -> Nothing
    msgs -> Just (Text.intercalate "; " msgs)

-- | Format a single field error, appending the suggestion when present.
formatFieldError :: EasyPostFieldError -> Text
formatFieldError fieldError =
  case fieldError.suggestion of
    Just suggestion
      | not (Text.null (Text.strip suggestion)) ->
          base <> " (suggestion: " <> suggestion <> ")"
    _ -> base
  where
    base = Text.strip fieldError.message
