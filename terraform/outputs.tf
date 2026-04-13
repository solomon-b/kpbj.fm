# ──────────────────────────────────────────────────────────────
# Outputs
# ──────────────────────────────────────────────────────────────

output "web_production_url" {
  description = "Production website URL"
  value       = "https://www.kpbj.fm"
}

output "web_staging_url" {
  description = "Staging website URL"
  value       = "https://staging.kpbj.fm"
}

output "stream_prod_droplet_ip" {
  description = "Public IPv4 address of the production streaming VPS"
  value       = digitalocean_droplet.stream_prod.ipv4_address
}

output "staging_droplet_ip" {
  description = "Public IPv4 address of the staging VPS"
  value       = digitalocean_droplet.staging.ipv4_address
}

output "cloudflare_zone_id" {
  description = "Cloudflare zone ID for kpbj.fm"
  value       = local.cloudflare_zone_id
  sensitive   = true
}

output "prod_spaces_endpoint" {
  description = "Production Spaces bucket endpoint"
  value       = "https://${digitalocean_spaces_bucket.prod_storage.name}.${digitalocean_spaces_bucket.prod_storage.region}.digitaloceanspaces.com"
}

output "staging_spaces_endpoint" {
  description = "Staging Spaces bucket endpoint"
  value       = "https://${digitalocean_spaces_bucket.staging_storage.name}.${digitalocean_spaces_bucket.staging_storage.region}.digitaloceanspaces.com"
}

output "pgbackrest_spaces_bucket" {
  description = "pgBackRest S3 backup bucket name"
  value       = digitalocean_spaces_bucket.pgbackrest.name
}

output "google_groups_sa_email" {
  description = "Google Groups service account email"
  value       = google_service_account.google_groups.email
}

output "stripe_webhook_secret_prod" {
  description = "Stripe webhook signing secret (production) — copy to secrets/prod-web.yaml"
  value       = stripe_webhook_endpoint.prod.secret
  sensitive   = true
}

output "stripe_webhook_secret_staging" {
  description = "Stripe webhook signing secret (staging) — copy to secrets/staging-web.yaml"
  value       = stripe_webhook_endpoint.staging.secret
  sensitive   = true
}
