-- Add Stripe Checkout Session ID column to orders table.
-- Used to track the Checkout Session that initiated the payment.
ALTER TABLE orders ADD COLUMN stripe_checkout_session_id TEXT;
COMMENT ON COLUMN orders.stripe_checkout_session_id IS 'Stripe Checkout Session ID for tracking the payment session';

CREATE INDEX idx_orders_stripe_checkout_session_id
  ON orders (stripe_checkout_session_id)
  WHERE stripe_checkout_session_id IS NOT NULL;

CREATE INDEX idx_orders_stripe_payment_intent_id
  ON orders (stripe_payment_intent_id)
  WHERE stripe_payment_intent_id IS NOT NULL;
