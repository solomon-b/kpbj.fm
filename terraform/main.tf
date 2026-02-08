# ──────────────────────────────────────────────────────────────
# KPBJ 95.9FM — Terraform Configuration
# ──────────────────────────────────────────────────────────────
#
# Infrastructure: DigitalOcean (streaming VPS with Icecast/Liquidsoap)
# and Cloudflare (DNS + proxy).
# Fly.io (web service) is managed via flyctl, not Terraform.
#
# See README.md for setup, import, and usage instructions.
# ──────────────────────────────────────────────────────────────

terraform {
  required_version = ">= 1.5"

  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.34"
    }

    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 5.0"
    }

    sops = {
      source  = "carlpett/sops"
      version = "~> 1.0"
    }
  }

  # Remote state in Tigris S3. Credentials are passed via
  # -backend-config at init time (see README.md).
  backend "s3" {
    bucket = "kpbj-terraform-state"
    key    = "terraform.tfstate"
    region = "auto"

    endpoints = {
      s3 = "https://fly.storage.tigris.dev"
    }

    # Tigris S3 compatibility
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    skip_requesting_account_id  = true
    skip_region_validation      = true
    skip_s3_checksum            = true

    # State contains secrets — encrypt at rest
    encrypt = true
  }
}

# ──────────────────────────────────────────────────────────────
# Providers
# ──────────────────────────────────────────────────────────────

provider "sops" {}

provider "cloudflare" {
  api_token = data.sops_file.secrets.data["cloudflare_api_token"]
}

provider "digitalocean" {
  token = data.sops_file.secrets.data["digitalocean_token"]
}
