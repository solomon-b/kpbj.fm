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

output "stream_staging_droplet_ip" {
  description = "Public IPv4 address of the staging streaming VPS"
  value       = digitalocean_droplet.stream_staging.ipv4_address
}

output "cloudflare_zone_id" {
  description = "Cloudflare zone ID for kpbj.fm"
  value       = local.cloudflare_zone_id
  sensitive   = true
}
