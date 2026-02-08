# ──────────────────────────────────────────────────────────────
# DigitalOcean — Streaming VPS (Production + Staging)
# ──────────────────────────────────────────────────────────────
#
# Separate droplets for production and staging streaming stacks
# (Icecast, Liquidsoap, Webhook) via Docker Compose. Nginx
# reverse proxy with certbot TLS is configured manually after
# initial provisioning.
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
  name     = "kpbj-stream-prod"
  region   = var.droplet_region
  size     = var.droplet_size
  image    = var.droplet_image
  ssh_keys = [for k in digitalocean_ssh_key.stream : k.fingerprint]

  user_data = <<-CLOUDINIT
    #!/bin/bash
    set -euo pipefail

    # Install Docker
    apt-get update
    apt-get install -y ca-certificates curl gnupg
    install -m 0755 -d /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    chmod a+r /etc/apt/keyrings/docker.gpg
    echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
      $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
      tee /etc/apt/sources.list.d/docker.list > /dev/null
    apt-get update
    apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

    # Create deploy user
    useradd -m -s /bin/bash -G docker deploy || true

    # Create streaming directory structure
    mkdir -p /opt/kpbj-stream
    chown deploy:deploy /opt/kpbj-stream

    # Install nginx
    apt-get install -y nginx certbot python3-certbot-nginx

    echo "Cloud-init provisioning complete."
  CLOUDINIT

  lifecycle {
    ignore_changes = [user_data]
  }
}

# ──────────────────────────────────────────────────────────────
# Staging Droplet
# ──────────────────────────────────────────────────────────────

resource "digitalocean_droplet" "stream_staging" {
  name     = "kpbj-stream-staging"
  region   = var.droplet_region
  size     = var.droplet_size_staging
  image    = var.droplet_image
  ssh_keys = [for k in digitalocean_ssh_key.stream : k.fingerprint]

  user_data = <<-CLOUDINIT
    #!/bin/bash
    set -euo pipefail

    # Install Docker
    apt-get update
    apt-get install -y ca-certificates curl gnupg
    install -m 0755 -d /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    chmod a+r /etc/apt/keyrings/docker.gpg
    echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
      $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
      tee /etc/apt/sources.list.d/docker.list > /dev/null
    apt-get update
    apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

    # Create deploy user
    useradd -m -s /bin/bash -G docker deploy || true

    # Create streaming directory structure
    mkdir -p /opt/kpbj-stream
    chown deploy:deploy /opt/kpbj-stream

    # Install nginx
    apt-get install -y nginx certbot python3-certbot-nginx

    echo "Cloud-init provisioning complete."
  CLOUDINIT

  lifecycle {
    ignore_changes = [user_data]
  }
}

# ──────────────────────────────────────────────────────────────
# Firewalls
# ──────────────────────────────────────────────────────────────

resource "digitalocean_firewall" "stream_prod" {
  name        = "kpbj-stream-prod-fw"
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

resource "digitalocean_firewall" "stream_staging" {
  name        = "kpbj-stream-staging-fw"
  droplet_ids = [digitalocean_droplet.stream_staging.id]

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
