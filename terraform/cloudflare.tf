# ──────────────────────────────────────────────────────────────
# Cloudflare — DNS + Proxy for kpbj.fm
# ──────────────────────────────────────────────────────────────

locals {
  cloudflare_zone_id = data.sops_file.secrets.data["cloudflare_zone_id"]
}

# ──────────────────────────────────────────────────────────────
# Production — Web Service (DigitalOcean VPS)
# ──────────────────────────────────────────────────────────────

# kpbj.fm → Production VPS (proxied through Cloudflare)
resource "cloudflare_dns_record" "root_a" {
  zone_id = local.cloudflare_zone_id
  name    = "kpbj.fm"
  type    = "A"
  content = digitalocean_droplet.stream_prod.ipv4_address
  proxied = true
  ttl     = 1
  comment = "Production web (DO VPS)"
}

# www.kpbj.fm → Production VPS (proxied through Cloudflare)
resource "cloudflare_dns_record" "www_a" {
  zone_id = local.cloudflare_zone_id
  name    = "www"
  type    = "A"
  content = digitalocean_droplet.stream_prod.ipv4_address
  proxied = true
  ttl     = 1
  comment = "Production web www (DO VPS)"
}

# ──────────────────────────────────────────────────────────────
# SSH Deploy Targets (DNS-only for GitHub Actions CI/CD)
# ──────────────────────────────────────────────────────────────

# ssh.kpbj.fm → Production VPS (DNS-only for SSH deploys)
resource "cloudflare_dns_record" "ssh_prod" {
  zone_id = local.cloudflare_zone_id
  name    = "ssh"
  type    = "A"
  content = digitalocean_droplet.stream_prod.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Production SSH deploy (DO VPS, DNS-only)"
}

# ssh.staging.kpbj.fm → Staging VPS (DNS-only for SSH deploys)
resource "cloudflare_dns_record" "ssh_staging" {
  zone_id = local.cloudflare_zone_id
  name    = "ssh.staging"
  type    = "A"
  content = digitalocean_droplet.staging.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Staging SSH deploy (DO VPS, DNS-only)"
}

# ──────────────────────────────────────────────────────────────
# Staging — Web Service (DigitalOcean VPS)
# ──────────────────────────────────────────────────────────────

# staging.kpbj.fm → DigitalOcean staging droplet (proxied through Cloudflare)
resource "cloudflare_dns_record" "staging_a" {
  zone_id = local.cloudflare_zone_id
  name    = "staging"
  type    = "A"
  content = digitalocean_droplet.staging.ipv4_address
  proxied = true
  ttl     = 1
  comment = "Staging web (DO VPS)"
}

# ──────────────────────────────────────────────────────────────
# Streaming — DigitalOcean VPS
# ──────────────────────────────────────────────────────────────

# stream.kpbj.fm → Production streaming VPS (DNS-only for audio streaming)
resource "cloudflare_dns_record" "stream" {
  zone_id = local.cloudflare_zone_id
  name    = "stream"
  type    = "A"
  content = digitalocean_droplet.stream_prod.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Production audio stream (DO VPS)"
}

# stream.staging.kpbj.fm → DigitalOcean staging droplet (DNS-only for audio streaming)
resource "cloudflare_dns_record" "stream_staging" {
  zone_id = local.cloudflare_zone_id
  name    = "stream.staging"
  type    = "A"
  content = digitalocean_droplet.staging.ipv4_address
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
  type    = "A"
  content = digitalocean_droplet.stream_prod.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Production file uploads (DO VPS, DNS-only for large uploads)"
}

resource "cloudflare_dns_record" "uploads_staging" {
  zone_id = local.cloudflare_zone_id
  name    = "uploads.staging"
  type    = "A"
  content = digitalocean_droplet.staging.ipv4_address
  proxied = false
  ttl     = 1
  comment = "Staging file uploads (DO VPS, DNS-only for large uploads)"
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
# SSL Certificate Validation
# ──────────────────────────────────────────────────────────────

resource "cloudflare_dns_record" "acme_challenge_root" {
  zone_id = local.cloudflare_zone_id
  name    = "_acme-challenge"
  type    = "TXT"
  content = "\"XVlM9mUEMJWveiNgfKDc1yOxJwBMUfy370VkxuwIrqk\""
  ttl     = 3600
  comment = "SSL cert validation for kpbj.fm"
}

resource "cloudflare_dns_record" "acme_challenge_www" {
  zone_id = local.cloudflare_zone_id
  name    = "_acme-challenge.www"
  type    = "TXT"
  content = "\"1O-zPj55UzJkBjn7ywlN1bD-m31qtC4I3Ec9KGMS-zU\""
  ttl     = 3600
  comment = "SSL cert validation for www.kpbj.fm"
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

# ──────────────────────────────────────────────────────────────
# WAF — Custom Rules (Free plan: 5 rule max)
# ──────────────────────────────────────────────────────────────
#
# Blocks common attack patterns at the Cloudflare edge before
# they reach the origin server. These complement fail2ban, which
# cannot ban Cloudflare-proxied traffic via nftables.
#
# Currently using 3 of 5 available rules.
# ──────────────────────────────────────────────────────────────

resource "cloudflare_ruleset" "waf_custom" {
  zone_id = local.cloudflare_zone_id
  name    = "KPBJ WAF custom rules"
  kind    = "zone"
  phase   = "http_request_firewall_custom"

  rules = [
    # ── Rule 1: Path traversal, LFI, and SSRF ──────────────────
    # Blocks directory traversal (../), local file inclusion
    # (/proc/, /etc/passwd), and cloud metadata SSRF attempts.
    {
      action      = "block"
      description = "Block path traversal, LFI, and SSRF"
      enabled     = true
      expression = join(" or ", [
        "(http.request.uri.path contains \"..\")",
        "(http.request.uri.path contains \"/proc/\")",
        "(http.request.uri.path contains \"/etc/passwd\")",
        "(http.request.uri.path contains \"/etc/shadow\")",
        "(http.request.uri contains \"169.254.169.254\")",
      ])
    },

    # ── Rule 2: Scanner/bot probe paths ─────────────────────────
    # Known-bad paths that only scanners and bots request.
    # Mirrors the fail2ban nginx-bad-paths filter, but blocks
    # at the edge. Uses lower() for case-insensitive matching.
    {
      action      = "block"
      description = "Block scanner and bot probe paths"
      enabled     = true
      expression = join(" or ", [
        "(lower(http.request.uri.path) contains \"/.env\")",
        "(lower(http.request.uri.path) contains \"/.git\")",
        "(lower(http.request.uri.path) contains \"/.htaccess\")",
        "(lower(http.request.uri.path) contains \"/.aws\")",
        "(lower(http.request.uri.path) contains \"/wp-login\")",
        "(lower(http.request.uri.path) contains \"/wp-admin\")",
        "(lower(http.request.uri.path) contains \"/wp-content\")",
        "(lower(http.request.uri.path) contains \"/wp-includes\")",
        "(lower(http.request.uri.path) contains \"/xmlrpc.php\")",
        "(lower(http.request.uri.path) contains \"/phpmy\")",
        "(lower(http.request.uri.path) contains \"/pma\")",
        "(lower(http.request.uri.path) contains \"/cgi-bin\")",
        "(lower(http.request.uri.path) contains \"/setup.php\")",
        "(lower(http.request.uri.path) contains \"/administrator\")",
        "(lower(http.request.uri.path) contains \"/config.php\")",
        "(lower(http.request.uri.path) contains \"/actuator\")",
        "(lower(http.request.uri.path) contains \"/api/v1/pods\")",
        "(lower(http.request.uri.path) contains \"/solr\")",
        "(lower(http.request.uri.path) contains \"/telescope\")",
        "(lower(http.request.uri.path) contains \"/_profiler\")",
        "(lower(http.request.uri.path) contains \"/eval-stdin\")",
      ])
    },

    # ── Rule 3: Malformed URI encoding ──────────────────────────
    # Blocks percent-encoded high bytes (0x80-0xFF) in the URI
    # path. All KPBJ paths are ASCII — high bytes in the path
    # are malicious probes (e.g. the \xAD invalid UTF-8 attacks).
    # Scoped to path only; request bodies are untouched so file
    # uploads work normally.
    {
      action      = "block"
      description = "Block non-ASCII percent-encoded bytes in URI path"
      enabled     = true
      expression  = "(http.request.uri.path matches \"%[89a-fA-F][0-9a-fA-F]\")"
    },
  ]
}
