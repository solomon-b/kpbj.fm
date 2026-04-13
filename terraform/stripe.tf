# ──────────────────────────────────────────────────────────────
# Stripe — Webhook endpoints
# ──────────────────────────────────────────────────────────────
#
# Manages Stripe webhook configuration as code. Each environment
# uses a separate Stripe mode (live/test) via provider aliases.
#
# To add a new event: append to enabled_events and terraform apply.
# ──────────────────────────────────────────────────────────────

resource "stripe_webhook_endpoint" "prod" {
  provider    = stripe.live
  url         = "https://www.kpbj.fm/api/webhooks/stripe"
  api_version = "2025-07-30.basil"

  enabled_events = [
    "checkout.session.completed",
  ]

  description = "KPBJ production webhook"
}

resource "stripe_webhook_endpoint" "staging" {
  provider    = stripe.test
  url         = "https://staging.kpbj.fm/api/webhooks/stripe"
  api_version = "2025-07-30.basil"

  enabled_events = [
    "checkout.session.completed",
  ]

  description = "KPBJ staging webhook"
}
