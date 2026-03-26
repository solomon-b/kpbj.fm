{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Store.Products.Create.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Products.Create.Post.Route (InlineCreateForm (..))
import API.Dashboard.Store.Products.Get.Templates (renderProductRow)
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwHandlerFailure)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans.Except (ExceptT)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cents qualified as Cents
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------
-- Validation

-- | Validate the inline create form, returning a 'Products.Insert' or an error message.
validateInlineCreateForm :: InlineCreateForm -> Either Text Products.Insert
validateInlineCreateForm form = do
  -- Sanitize and validate name
  let sanitizedName = Sanitize.sanitizeTitle form.icfName
  validName <- case Sanitize.validateContentLength 200 sanitizedName of
    Left err -> Left (Sanitize.displayContentValidationError err)
    Right n -> Right n

  -- Parse price
  basePriceCents <- case readMaybe @Scientific (Text.unpack form.icfBasePriceDollars) of
    Just d | d >= 0 -> Right (Cents.dollarsToCents d)
    _ -> Left ("Invalid price: \"" <> form.icfBasePriceDollars <> "\" is not a valid amount")

  -- Parse inventory count
  inventoryCount <- case form.icfInventoryCount of
    t | Text.null (Text.strip t) -> Right 0
    t -> Sanitize.parseNonNegativeInt t

  -- Derive slug from name
  let Slug.Slug slugText = Slug.mkSlug validName

  -- Sanitize optional category
  let mCategory =
        case form.icfCategory of
          Nothing -> Nothing
          Just cat ->
            let sanitized = Sanitize.sanitizePlainText cat
             in if Text.null (Text.strip sanitized) then Nothing else Just sanitized

  Right
    Products.Insert
      { Products.piName = validName,
        Products.piSlug = slugText,
        Products.piDescription = "",
        Products.piBasePriceCents = basePriceCents,
        Products.piWeightOz = 0,
        Products.piCategory = mCategory,
        Products.piInventoryCount = inventoryCount,
        Products.piIsActive = False,
        Products.piSortOrder = 0
      }

--------------------------------------------------------------------------------

-- | Business logic: validate form, insert product, query it back, render row.
action ::
  User.Model ->
  UserMetadata.Model ->
  InlineCreateForm ->
  ExceptT HandlerError AppM (Lucid.Html ())
action _user _userMetadata form = do
  -- 1. Validate form
  productInsert <- requireRight id (validateInlineCreateForm form)

  -- 2. Check for slug conflict
  mExisting <-
    fromRightM throwDatabaseError $
      execQuery (Products.getBySlug productInsert.piSlug)
  case mExisting of
    Just _ -> throwHandlerFailure "A product with a similar name already exists. Please choose a different name."
    Nothing -> pure ()

  -- 3. Insert product
  mProductId <-
    fromRightM throwDatabaseError $
      execQuery (Products.insertProduct productInsert)

  productId <- case mProductId of
    Just pid -> pure pid
    Nothing -> throwHandlerFailure "Product insert returned Nothing"

  -- 4. Query the inserted product back to render the row
  mProduct <-
    fromRightM throwDatabaseError $
      execQuery (Products.getById productId)

  productModel <- case mProduct of
    Just p -> pure p
    Nothing -> throwHandlerFailure "Inserted product not found after insert"

  -- 5. Log and return row HTML + OOB success banner
  Log.logInfo "Inline product created successfully" productId

  pure $ do
    renderProductRow productModel
    renderBanner Success "Product Created" "Product created successfully."

-- | Servant handler: thin glue composing auth + action.
handler ::
  Maybe Cookie ->
  InlineCreateForm ->
  AppM (Lucid.Html ())
handler cookie form =
  handleBannerErrors "Inline product create" $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create products." userMetadata
    action user userMetadata form
