# ──────────────────────────────────────────────────────────────
# Cloudflare — DNS + Proxy for kpbj.fm
# ──────────────────────────────────────────────────────────────

locals {
  cloudflare_zone_id = data.sops_file.secrets.data["cloudflare_zone_id"]
}

# ──────────────────────────────────────────────────────────────
# Production — Web Service (Fly.io)
# ──────────────────────────────────────────────────────────────

# kpbj.fm → Fly.io (AAAA only — Cloudflare proxy handles IPv4 at the edge)
resource "cloudflare_dns_record" "root_aaaa" {
  zone_id = local.cloudflare_zone_id
  name    = "kpbj.fm"
  type    = "AAAA"
  content = "2a09:8280:1::c2:ea7e:0"
  proxied = true
  ttl     = 1 # Auto when proxied
  comment = "Production web (Fly.io)"
}

# www.kpbj.fm → Fly.io
resource "cloudflare_dns_record" "www_aaaa" {
  zone_id = local.cloudflare_zone_id
  name    = "www"
  type    = "AAAA"
  content = "2a09:8280:1::c2:ea7e:0"
  proxied = true
  ttl     = 1
  comment = "Production web www (Fly.io)"
}

# ──────────────────────────────────────────────────────────────
# Staging — Web Service (DigitalOcean VPS)
# ──────────────────────────────────────────────────────────────

# staging.kpbj.fm → DigitalOcean staging droplet (DNS-only for ACME HTTP-01)
resource "cloudflare_dns_record" "staging_a" {
  zone_id = local.cloudflare_zone_id
  name    = "staging"
  type    = "A"
  content = digitalocean_droplet.stream_staging.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Staging web (DO VPS)"
}

# ──────────────────────────────────────────────────────────────
# Streaming — DigitalOcean VPS
# ──────────────────────────────────────────────────────────────

# stream.kpbj.fm → Production streaming VPS (DNS-only for audio streaming)
# TODO: Switch to digitalocean_droplet.stream_prod.ipv4_address after NixOS cutover
resource "cloudflare_dns_record" "stream" {
  zone_id = local.cloudflare_zone_id
  name    = "stream"
  type    = "A"
  content = "167.172.122.186"
  proxied = false
  ttl     = 1
  comment = "Production audio stream (legacy VPS)"
}

# stream.staging.kpbj.fm → DigitalOcean staging droplet (DNS-only for audio streaming)
resource "cloudflare_dns_record" "stream_staging" {
  zone_id = local.cloudflare_zone_id
  name    = "stream.staging"
  type    = "A"
  content = digitalocean_droplet.stream_staging.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Staging audio stream (DO)"
}

# ──────────────────────────────────────────────────────────────
# Other Services (separate VPS at 69.42.217.182)
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "libretime" {
  zone_id = local.cloudflare_zone_id
  name    = "libretime"
  type    = "A"
  content = var.services_vps_ip
  proxied = false
  ttl     = 1
  comment = "LibreTime (services VPS)"
}

resource "cloudflare_dns_record" "planka" {
  zone_id = local.cloudflare_zone_id
  name    = "planka"
  type    = "A"
  content = var.services_vps_ip
  proxied = true
  ttl     = 1
  comment = "Planka project board (services VPS)"
}

# ──────────────────────────────────────────────────────────────
# File Uploads
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "uploads" {
  zone_id = local.cloudflare_zone_id
  name    = "uploads"
  type    = "CNAME"
  content = "m2y621y.kpbj-fm.fly.dev"
  proxied = false
  ttl     = 1
  comment = "Production file uploads (Fly.io)"
}

resource "cloudflare_dns_record" "uploads_staging" {
  zone_id = local.cloudflare_zone_id
  name    = "uploads.staging"
  type    = "A"
  content = digitalocean_droplet.stream_staging.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Staging file uploads (DO VPS)"
}

# ──────────────────────────────────────────────────────────────
# Fly.io TLS Certificate Validation (Production only)
# ──────────────────────────────────────────────────────────────
#
# CNAME delegation to Fly's DNS for ACME challenge resolution.
# Staging uses NixOS ACME (HTTP-01) on the VPS directly.
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "acme_challenge_root" {
  zone_id = local.cloudflare_zone_id
  name    = "_acme-challenge"
  type    = "CNAME"
  content = "www.kpbj.fm.m2y621y.flydns.net"
  proxied = false
  ttl     = 1
  comment = "TLS cert validation for kpbj.fm (Fly.io)"
}

resource "cloudflare_dns_record" "acme_challenge_uploads" {
  zone_id = local.cloudflare_zone_id
  name    = "_acme-challenge.uploads"
  type    = "CNAME"
  content = "uploads.kpbj.fm.m2y621y.flydns.net"
  proxied = false
  ttl     = 1
  comment = "TLS cert validation for uploads (Fly.io)"
}

# ──────────────────────────────────────────────────────────────
# Email — Google Workspace
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "mx_primary" {
  zone_id  = local.cloudflare_zone_id
  name     = "kpbj.fm"
  type     = "MX"
  content  = "aspmx.l.google.com"
  priority = 1
  ttl      = 3600
  comment  = "Google Workspace MX (primary)"
}

resource "cloudflare_dns_record" "mx_alt1" {
  zone_id  = local.cloudflare_zone_id
  name     = "kpbj.fm"
  type     = "MX"
  content  = "alt1.aspmx.l.google.com"
  priority = 5
  ttl      = 3600
  comment  = "Google Workspace MX (alt1)"
}

resource "cloudflare_dns_record" "mx_alt2" {
  zone_id  = local.cloudflare_zone_id
  name     = "kpbj.fm"
  type     = "MX"
  content  = "alt2.aspmx.l.google.com"
  priority = 5
  ttl      = 3600
  comment  = "Google Workspace MX (alt2)"
}

resource "cloudflare_dns_record" "mx_alt3" {
  zone_id  = local.cloudflare_zone_id
  name     = "kpbj.fm"
  type     = "MX"
  content  = "alt3.aspmx.l.google.com"
  priority = 10
  ttl      = 3600
  comment  = "Google Workspace MX (alt3)"
}

resource "cloudflare_dns_record" "mx_alt4" {
  zone_id  = local.cloudflare_zone_id
  name     = "kpbj.fm"
  type     = "MX"
  content  = "alt4.aspmx.l.google.com"
  priority = 10
  ttl      = 3600
  comment  = "Google Workspace MX (alt4)"
}

# ──────────────────────────────────────────────────────────────
# Email Authentication (SPF, DKIM, DMARC)
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "spf" {
  zone_id = local.cloudflare_zone_id
  name    = "kpbj.fm"
  type    = "TXT"
  content = "\"v=spf1 include:_spf.google.com include:servers.mcsv.net ~all\""
  ttl     = 3600
  comment = "SPF (Google + Mailchimp)"
}

resource "cloudflare_dns_record" "dmarc" {
  zone_id = local.cloudflare_zone_id
  name    = "_dmarc"
  type    = "TXT"
  content = "\"v=DMARC1; p=none;\""
  ttl     = 3600
  comment = "DMARC policy"
}

resource "cloudflare_dns_record" "dkim_default" {
  zone_id = local.cloudflare_zone_id
  name    = "default._domainkey"
  type    = "TXT"
  content = "\"v=DKIM1; h=sha256; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDukAccDq5yKLWEtQhVGQcHohiS1GbEIHrij9ZmqUfinz5tJSdunBFOUIr2SwerFBJGraEC21Pz6abINUA/LSlqbZvusay4M1Uuo1wabhtFPydV+6JXKqLpxOioapAF2b5MpG2akxXNhW6WRXBd5PbRNHrA0Fv8kCcgx+lERUcfowIDAQAB\""
  ttl     = 3600
  comment = "DKIM (Google Workspace)"
}

# Mailchimp DKIM signing
resource "cloudflare_dns_record" "dkim_k2" {
  zone_id = local.cloudflare_zone_id
  name    = "k2._domainkey"
  type    = "CNAME"
  content = "dkim2.mcsv.net"
  proxied = false
  ttl     = 1
  comment = "DKIM (Mailchimp)"
}

resource "cloudflare_dns_record" "dkim_k3" {
  zone_id = local.cloudflare_zone_id
  name    = "k3._domainkey"
  type    = "CNAME"
  content = "dkim3.mcsv.net"
  proxied = false
  ttl     = 1
  comment = "DKIM (Mailchimp)"
}

# ──────────────────────────────────────────────────────────────
# Domain Verification
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "google_site_verification" {
  zone_id = local.cloudflare_zone_id
  name    = "kpbj.fm"
  type    = "TXT"
  content = "\"google-site-verification=r4dp_pmvNZF2YES0FbousJddL0DHv0TN7xiwwmv2X5s\""
  ttl     = 3600
  comment = "Google site verification"
}

resource "cloudflare_dns_record" "nms_verification" {
  zone_id = local.cloudflare_zone_id
  name    = "kpbj.fm"
  type    = "TXT"
  content = "\"nms-domain-verification=542721151170\""
  ttl     = 3600
  comment = "NMS domain verification"
}
