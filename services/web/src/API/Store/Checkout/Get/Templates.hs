{-# LANGUAGE QuasiQuotes #-}

-- | Template for the public store checkout page.
--
-- Renders a multi-step checkout shell. The form collects contact info
-- and a shipping address. An Alpine.js component reads the cart from
-- @Alpine.store('cart')@, displays an order summary, and POSTs to the
-- shipping-rates endpoint via HTMX. The Stripe embedded checkout mounts
-- into @#stripe-checkout@ once a session is created.
module API.Store.Checkout.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (storeApiLinks, storeLinks)
import API.Types (StoreApiRoutes (..), StoreRoutes (..))
import Component.PageHeader (pageHeader)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_, tablet)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)
import Lucid.HTMX
import Servant.Links qualified as Links
import Stripe.Types (StripePublishableKey (..))

--------------------------------------------------------------------------------
-- URL helpers

shippingRatesUrl :: Links.URI
shippingRatesUrl = Links.linkURI storeApiLinks.shippingRates

createSessionUrl :: Links.URI
createSessionUrl = Links.linkURI storeApiLinks.createSession

storeListUrl :: Links.URI
storeListUrl = Links.linkURI storeLinks.list

--------------------------------------------------------------------------------

-- | Checkout page template.
--
-- Renders a two-column layout: left column holds the checkout form,
-- right column holds the order summary. All cart reads and the
-- shipping-rate submission are handled by the Alpine.js component.
-- When a Stripe publishable key is available, loads Stripe.js so the
-- embedded checkout can mount into @#stripe-checkout@.
template :: Maybe StripePublishableKey -> Lucid.Html ()
template mStripeKey = do
  pageHeader "CHECKOUT"

  -- Load Stripe.js when a key is configured
  case mStripeKey of
    Nothing -> mempty
    Just _ ->
      Lucid.script_
        [Lucid.src_ "https://js.stripe.com/v3/"]
        ("" :: Text)

  Lucid.div_
    [ xData_ (checkoutAlpineState mStripeKey),
      xInit_ "initCheckout()",
      class_ $ base ["w-full", "max-w-4xl", "mx-auto"]
    ]
    $ do
      -- Empty cart guard
      Lucid.div_ [xShow_ "isEmpty"] $ do
        Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb4, "text-center"]] "Your cart is empty."
        Lucid.a_
          [ Lucid.href_ [i|/#{storeListUrl}|],
            hxGet_ [i|/#{storeListUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.linkText]
          ]
          "Continue Shopping"

      -- Checkout steps (only one visible at a time)
      Lucid.div_
        [ xShow_ "!isEmpty",
          class_ $ base ["max-w-lg", "mx-auto"]
        ]
        $ do
          Lucid.div_ [xShow_ "step === 'address'"] $ do
            renderContactSection
            renderShippingSection
          Lucid.div_ [xShow_ "step === 'rates'"] renderShippingRatesSection
          Lucid.div_ [xShow_ "step === 'payment'"] renderPaymentSection

--------------------------------------------------------------------------------
-- Form sections

renderContactSection :: Lucid.Html ()
renderContactSection =
  Lucid.section_ [class_ $ base [Tokens.mb8]] $ do
    Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4, "uppercase"]] "Contact"
    renderField "email" "Email" "email" "your@email.com" True $
      Lucid.input_
        [ Lucid.type_ "email",
          Lucid.id_ "email",
          Lucid.name_ "email",
          xModel_ "form.email",
          Lucid.placeholder_ "your@email.com",
          Lucid.required_ "",
          Lucid.class_ inputClass
        ]

renderShippingSection :: Lucid.Html ()
renderShippingSection =
  Lucid.section_ [class_ $ base [Tokens.mb8]] $ do
    Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4, "uppercase"]] "Shipping Address"

    -- First name / Last name row
    Lucid.div_
      [ class_ $ do
          base ["flex", "flex-col", Tokens.gap4]
          tablet ["flex-row"]
      ]
      $ do
        Lucid.div_ [class_ $ base ["flex-1"]] $
          renderField "firstName" "First Name" "text" "Jane" True $
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.id_ "firstName",
                Lucid.name_ "firstName",
                xModel_ "form.firstName",
                Lucid.placeholder_ "Jane",
                Lucid.required_ "",
                Lucid.class_ inputClass
              ]
        Lucid.div_ [class_ $ base ["flex-1"]] $
          renderField "lastName" "Last Name" "text" "Smith" True $
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.id_ "lastName",
                Lucid.name_ "lastName",
                xModel_ "form.lastName",
                Lucid.placeholder_ "Smith",
                Lucid.required_ "",
                Lucid.class_ inputClass
              ]

    renderField "address1" "Address" "text" "123 Main St" True $
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.id_ "address1",
          Lucid.name_ "address1",
          xModel_ "form.address1",
          Lucid.placeholder_ "123 Main St",
          Lucid.required_ "",
          Lucid.class_ inputClass
        ]

    renderField "address2" "Apt, Suite, etc. (optional)" "text" "Apt 4B" False $
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.id_ "address2",
          Lucid.name_ "address2",
          xModel_ "form.address2",
          Lucid.placeholder_ "Apt 4B",
          Lucid.class_ inputClass
        ]

    -- City / State / Zip row
    Lucid.div_
      [ class_ $ do
          base ["flex", "flex-col", Tokens.gap4]
          tablet ["flex-row"]
      ]
      $ do
        Lucid.div_ [class_ $ base ["flex-1"]] $
          renderField "city" "City" "text" "Sun Valley" True $
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.id_ "city",
                Lucid.name_ "city",
                xModel_ "form.city",
                Lucid.placeholder_ "Sun Valley",
                Lucid.required_ "",
                Lucid.class_ inputClass
              ]
        Lucid.div_ [class_ $ base ["w-full", "md:w-32", "flex-shrink-0"]] $
          renderField "state" "State" "text" "CA" True $
            Lucid.select_
              [ Lucid.id_ "state",
                Lucid.name_ "state",
                xModel_ "form.state",
                Lucid.required_ "",
                Lucid.class_ selectClass
              ]
              usStateOptions
        Lucid.div_ [class_ $ base ["w-full", "md:w-28", "flex-shrink-0"]] $
          renderField "zip" "ZIP" "text" "91352" True $
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.id_ "zip",
                Lucid.name_ "zip",
                xModel_ "form.zip",
                Lucid.placeholder_ "91352",
                Lucid.required_ "",
                Lucid.class_ inputClass
              ]

    -- Get Shipping Rates button
    Lucid.div_ [class_ $ base [Tokens.mt4]] $
      Lucid.button_
        [ Lucid.type_ "button",
          xOn_ "click" "fetchShippingRates()",
          makeAttributes ":disabled" "!isAddressComplete",
          class_ $ base [Tokens.buttonPrimary, "w-full", "uppercase", "disabled:opacity-50", "disabled:cursor-not-allowed"]
        ]
        "Get Shipping Rates"

renderShippingRatesSection :: Lucid.Html ()
renderShippingRatesSection =
  Lucid.div_
    [ Lucid.id_ "shipping-rates",
      class_ $ base [Tokens.mb8]
    ]
    mempty

renderPaymentSection :: Lucid.Html ()
renderPaymentSection =
  Lucid.div_
    [ Lucid.id_ "stripe-checkout",
      class_ $ base [Tokens.mb8]
    ]
    mempty

--------------------------------------------------------------------------------
-- Field helper

-- | Render a labeled form field wrapper.
renderField ::
  -- | Field id
  Text ->
  -- | Label text
  Text ->
  -- | Input type (unused, provided for caller clarity)
  Text ->
  -- | Placeholder (unused here, caller sets on input)
  Text ->
  -- | Whether field is required (controls label marker)
  Bool ->
  -- | The input element itself
  Lucid.Html () ->
  Lucid.Html ()
renderField fieldId labelText _type _placeholder isRequired inputEl =
  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    Lucid.label_
      [ Lucid.for_ fieldId,
        Lucid.class_ "fb-label"
      ]
      $ do
        Lucid.toHtml labelText
        if isRequired
          then Lucid.span_ [class_ $ base [Tokens.fgMuted, "ml-1"]] "*"
          else mempty
    inputEl

-- | Standard input field class — uses the form builder's semantic class
-- so inputs are styled consistently with forms throughout the site.
inputClass :: Text
inputClass = "fb-input"

-- | Standard select field class.
selectClass :: Text
selectClass = "fb-select"

--------------------------------------------------------------------------------
-- US state options

usStateOptions :: Lucid.Html ()
usStateOptions = do
  Lucid.option_ [Lucid.value_ ""] "-- State --"
  mapM_ stateOption usStates
  where
    stateOption :: (Text, Text) -> Lucid.Html ()
    stateOption (abbr, name) =
      Lucid.option_ [Lucid.value_ abbr] (Lucid.toHtml name)

usStates :: [(Text, Text)]
usStates =
  [ ("AL", "Alabama"),
    ("AK", "Alaska"),
    ("AZ", "Arizona"),
    ("AR", "Arkansas"),
    ("CA", "California"),
    ("CO", "Colorado"),
    ("CT", "Connecticut"),
    ("DE", "Delaware"),
    ("FL", "Florida"),
    ("GA", "Georgia"),
    ("HI", "Hawaii"),
    ("ID", "Idaho"),
    ("IL", "Illinois"),
    ("IN", "Indiana"),
    ("IA", "Iowa"),
    ("KS", "Kansas"),
    ("KY", "Kentucky"),
    ("LA", "Louisiana"),
    ("ME", "Maine"),
    ("MD", "Maryland"),
    ("MA", "Massachusetts"),
    ("MI", "Michigan"),
    ("MN", "Minnesota"),
    ("MS", "Mississippi"),
    ("MO", "Missouri"),
    ("MT", "Montana"),
    ("NE", "Nebraska"),
    ("NV", "Nevada"),
    ("NH", "New Hampshire"),
    ("NJ", "New Jersey"),
    ("NM", "New Mexico"),
    ("NY", "New York"),
    ("NC", "North Carolina"),
    ("ND", "North Dakota"),
    ("OH", "Ohio"),
    ("OK", "Oklahoma"),
    ("OR", "Oregon"),
    ("PA", "Pennsylvania"),
    ("RI", "Rhode Island"),
    ("SC", "South Carolina"),
    ("SD", "South Dakota"),
    ("TN", "Tennessee"),
    ("TX", "Texas"),
    ("UT", "Utah"),
    ("VT", "Vermont"),
    ("VA", "Virginia"),
    ("WA", "Washington"),
    ("WV", "West Virginia"),
    ("WI", "Wisconsin"),
    ("WY", "Wyoming"),
    ("DC", "District of Columbia")
  ]

--------------------------------------------------------------------------------
-- Alpine.js state

-- | Alpine.js component state for the checkout page.
--
-- Reads cart items from @Alpine.store('cart')@, tracks form fields,
-- and submits address + cart data to the shipping-rates endpoint.
-- When @showPayment@ is true the @#stripe-checkout@ div becomes visible
-- for the Stripe embedded checkout to mount into.
checkoutAlpineState :: Maybe StripePublishableKey -> Text
checkoutAlpineState mStripeKey =
  [i|{
  form: {
    email: '',
    firstName: '',
    lastName: '',
    address1: '',
    address2: '',
    city: '',
    state: '',
    zip: ''
  },
  step: 'address',
  summaryLoading: false,
  selectedRateId: null,
  selectedShipmentId: null,
  stripeKey: #{stripeKeyJs mStripeKey},
  stripeCheckout: null,
  get cartItems() { return Alpine.store('cart').items; },
  get isEmpty() { return this.cartItems.length === 0; },
  get isAddressComplete() {
    const f = this.form;
    return f.email !== '' && f.firstName !== '' && f.lastName !== ''
      && f.address1 !== '' && f.city !== '' && f.state !== '' && f.zip !== '';
  },
  initCheckout() {
    this.$el.addEventListener('shipping-rate-selected', (e) => {
      this.selectedRateId = e.detail.rateId;
      this.selectedShipmentId = e.detail.shipmentId || document.querySelector('input[name="shipment_id"]')?.value;
      this.createSession();
    });
  },
  fetchShippingRates() {
    if (!this.isAddressComplete) return;
    const payload = {
      email: this.form.email,
      first_name: this.form.firstName,
      last_name: this.form.lastName,
      address_line1: this.form.address1,
      address_line2: this.form.address2 || '',
      city: this.form.city,
      state: this.form.state,
      zip: this.form.zip,
      cart: this.cartItems
    };
    fetch('/#{shippingRatesUrl}', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    })
    .then(r => r.text())
    .then(html => {
      document.getElementById('shipping-rates').innerHTML = html;
      this.step = 'rates';
    })
    .catch(err => {
      document.getElementById('shipping-rates').innerHTML =
        '<div class="border-2 border-red-500 p-4">Failed to load shipping rates. Please try again.</div>';
    });
  },
  createSession() {
    if (!this.selectedRateId || !this.stripeKey) return;
    const payload = {
      email: this.form.email,
      first_name: this.form.firstName,
      last_name: this.form.lastName,
      address_line1: this.form.address1,
      address_line2: this.form.address2 || '',
      city: this.form.city,
      state: this.form.state,
      zip: this.form.zip,
      shipping_rate_id: this.selectedRateId,
      shipping_shipment_id: this.selectedShipmentId,
      cart: this.cartItems
    };
    fetch('/#{createSessionUrl}', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    })
    .then(r => {
      if (!r.ok) return r.json().then(e => { throw new Error(e.error || 'Checkout failed'); });
      return r.json();
    })
    .then(async (data) => {
      this.step = 'payment';
      const stripe = Stripe(this.stripeKey);
      this.stripeCheckout = await stripe.initEmbeddedCheckout({ clientSecret: data.client_secret });
      this.stripeCheckout.mount('\#stripe-checkout');
    })
    .catch(err => {
      alert(err.message || 'Something went wrong. Please try again.');
    });
  }
}|]
  where
    stripeKeyJs Nothing = "null" :: Text
    stripeKeyJs (Just (StripePublishableKey k)) = [i|'#{k}'|]
