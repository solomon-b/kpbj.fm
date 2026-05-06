# ──────────────────────────────────────────────────────────────
# DigitalOcean — Streaming VPS (Production + Staging)
# ──────────────────────────────────────────────────────────────
#
# Separate droplets for production and staging streaming stacks
# (Icecast, Liquidsoap, Webhook). Droplets start as Ubuntu and
# are automatically converted to NixOS via nixos-infect on first
# boot. After infect completes, run `just nixos-setup` then
# `just nixos-deploy-*` to push the declarative NixOS config
# (Podman containers, nginx + ACME).
# ──────────────────────────────────────────────────────────────

resource "digitalocean_ssh_key" "stream" {
  for_each   = var.ssh_public_keys
  name       = "kpbj-stream-${each.key}"
  public_key = each.value
}

# ──────────────────────────────────────────────────────────────
# Production Droplet
# ──────────────────────────────────────────────────────────────

resource "digitalocean_droplet" "stream_prod" {
  name       = "kpbj-prod"
  region     = var.droplet_region
  size       = var.droplet_size
  image      = var.droplet_image
  ssh_keys   = [for k in digitalocean_ssh_key.stream : k.fingerprint]
  backups    = true

  # nixos-infect converts Ubuntu to NixOS on first boot.
  # After infect completes (~5-10min), run: just nixos-setup root@<ip> prod
  user_data = <<-NIXOSINFECT
    #!/bin/bash
    umount /boot/efi 2>/dev/null || true
    curl https://raw.githubusercontent.com/elitak/nixos-infect/36f48d8feb89ca508261d7390355144fc0048932/nixos-infect | \
      PROVIDER=digitalocean NIX_CHANNEL=nixos-25.05 bash 2>&1 | tee /tmp/nixos-infect.log
  NIXOSINFECT

  lifecycle {
    ignore_changes = [user_data]
  }
}

# ──────────────────────────────────────────────────────────────
# Staging Droplet
# ──────────────────────────────────────────────────────────────

resource "digitalocean_droplet" "staging" {
  name     = "kpbj-staging"
  region   = var.droplet_region
  size     = var.droplet_size_staging
  image    = var.droplet_image
  ssh_keys = [for k in digitalocean_ssh_key.stream : k.fingerprint]

  # nixos-infect converts Ubuntu to NixOS on first boot.
  # After infect completes (~5-10min), run: just nixos-setup root@<ip> staging
  user_data = <<-NIXOSINFECT
    #!/bin/bash
    umount /boot/efi 2>/dev/null || true
    curl https://raw.githubusercontent.com/elitak/nixos-infect/36f48d8feb89ca508261d7390355144fc0048932/nixos-infect | \
      PROVIDER=digitalocean NIX_CHANNEL=nixos-25.05 bash 2>&1 | tee /tmp/nixos-infect.log
  NIXOSINFECT

  lifecycle {
    ignore_changes = [user_data]
  }
}

# ──────────────────────────────────────────────────────────────
# Spaces (S3-compatible object storage)
# ──────────────────────────────────────────────────────────────

resource "digitalocean_spaces_bucket" "prod_storage" {
  name   = "production-kpbj-storage"
  region = "sfo3"
  acl    = "public-read"
}

resource "digitalocean_spaces_bucket" "staging_storage" {
  name   = "staging-kpbj-storage"
  region = "sfo3"
  acl    = "public-read"
}

resource "digitalocean_spaces_bucket" "pgbackrest" {
  name   = "kpbj-pgbackrest"
  region = "sfo3"
  acl    = "private"
}

# ──────────────────────────────────────────────────────────────
# Firewalls
# ──────────────────────────────────────────────────────────────

resource "digitalocean_firewall" "stream_prod" {
  name        = "kpbj-prod-fw"
  droplet_ids = [digitalocean_droplet.stream_prod.id]

  # SSH
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTP (redirect to HTTPS)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "80"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTPS (nginx → icecast)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "443"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # ICMP (ping, Path MTU Discovery)
  inbound_rule {
    protocol         = "icmp"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # Allow all outbound
  outbound_rule {
    protocol              = "tcp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "udp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "icmp"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}

# ──────────────────────────────────────────────────────────────
# Project — groups KPBJ resources in the DO control panel
# ──────────────────────────────────────────────────────────────
#
# Project assignment is metadata-only on DO's side: assigning a
# resource here moves it out of whatever project currently owns
# it (including the account default project) without touching
# the resource itself — no reboot, no IP change, no downtime.
# Firewalls and SSH keys are not project-assignable.

resource "digitalocean_project" "kpbj" {
  name        = "kpbj-fm"
  description = "KPBJ 95.9FM community radio — streaming VPS, web app, object storage"
  purpose     = "Web Application"

  resources = [
    digitalocean_droplet.stream_prod.urn,
    digitalocean_droplet.staging.urn,
    digitalocean_spaces_bucket.prod_storage.urn,
    digitalocean_spaces_bucket.staging_storage.urn,
    digitalocean_spaces_bucket.pgbackrest.urn,
  ]
}

resource "digitalocean_firewall" "staging" {
  name        = "kpbj-staging-fw"
  droplet_ids = [digitalocean_droplet.staging.id]

  # SSH
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTP (redirect to HTTPS)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "80"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTPS (nginx → icecast)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "443"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # ICMP (ping, Path MTU Discovery)
  inbound_rule {
    protocol         = "icmp"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # Allow all outbound
  outbound_rule {
    protocol              = "tcp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "udp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "icmp"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}
