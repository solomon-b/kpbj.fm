# ──────────────────────────────────────────────────────────────
# DigitalOcean — Streaming VPS
# ──────────────────────────────────────────────────────────────

variable "droplet_region" {
  description = "DigitalOcean region for the streaming VPS"
  type        = string
  default     = "sfo3"
}

variable "droplet_size" {
  description = "Droplet size slug (production)"
  type        = string
  default     = "s-1vcpu-1gb"
}

variable "droplet_size_staging" {
  description = "Droplet size slug (staging)"
  type        = string
  default     = "s-1vcpu-1gb"
}

variable "droplet_image" {
  description = "Droplet base image"
  type        = string
  default     = "ubuntu-24-04-x64"
}

variable "services_vps_ip" {
  description = "IPv4 address of the services VPS (libretime, planka)"
  type        = string
  default     = "69.42.217.182"
}

variable "ssh_public_keys" {
  description = "SSH public keys for VPS access, keyed by machine name"
  type        = map(string)
  default = {
    nightshade       = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp solomon@nightshade"
    lorean           = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o solomon@lorean"
    voice-of-evening = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHWnMYdY2wfu05WThiGKlNK8aCX3HmNyQds8MOoSM+v solomon@voice-of-evening"
  }
}
