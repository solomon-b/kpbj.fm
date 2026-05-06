# KPBJ Terraform Infrastructure

Terraform configuration for KPBJ 95.9FM infrastructure: DigitalOcean streaming VPS and Cloudflare DNS + proxy.

## Prerequisites

- [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5
- [just](https://github.com/casey/just) — task runner
- [sops](https://github.com/getsops/sops) + [age](https://github.com/FiloSottile/age) — secrets management
- [jq](https://jqlang.github.io/jq/) — JSON processing (for import script)

## Setup

```bash
# 1. Initialize providers (passes S3 backend credentials automatically)
just tf-init

# 2. Import existing Cloudflare resources (see below)

# 3. Verify state matches reality
just tf-plan
```

The S3 backend requires `-backend-config` credentials for DigitalOcean Spaces. `just tf-init` handles this using SOPS-encrypted keys from `secrets/terraform.yaml`.

**State locking is not available.** DigitalOcean Spaces does not support DynamoDB-style lock tables, so concurrent `terraform apply` runs can corrupt state. Only one person should run plan/apply at a time.

## Importing Existing Resources

Since Cloudflare DNS records already exist, run the import script to bring them into Terraform state:

```bash
# 1. Generate declarative import blocks
./scripts/tf-import.sh

# 2. Review the generated file
cat terraform/imports.tf

# 3. Plan — should show imports with no other changes
just tf-plan

# 4. Apply — imports resources into state
just tf-apply

# 5. Clean up the ephemeral file
rm terraform/imports.tf
```

DigitalOcean resources (droplets, firewalls, SSH keys) are provisioned fresh by Terraform — they are not imported.

## Secrets

Sensitive values (API tokens, zone IDs) are stored in `secrets/terraform.yaml`, encrypted with SOPS + age.

```bash
# Edit secrets (opens in $EDITOR, re-encrypts on save)
just tf-edit-secrets
```

## Day-to-Day Usage

```bash
# Preview changes
just tf-plan

# Apply changes
just tf-apply

# Show current state
just tf-show

# Format files
just tf-fmt

# Validate configuration
just tf-validate
```

## What Terraform Manages

| Resource                             | Provider     | File              |
|--------------------------------------|--------------|-------------------|
| DigitalOcean droplets (prod + staging) | digitalocean | `digitalocean.tf` |
| DigitalOcean firewalls (prod + staging) | digitalocean | `digitalocean.tf` |
| DigitalOcean SSH keys                | digitalocean | `digitalocean.tf` |
| DigitalOcean Spaces buckets (prod + staging) | digitalocean | `digitalocean.tf` |
| DigitalOcean project (`kpbj-fm`)     | digitalocean | `digitalocean.tf` |
| Cloudflare DNS records               | cloudflare   | `cloudflare.tf`   |
| Cloudflare proxy settings            | cloudflare   | `cloudflare.tf`   |
| GCP Admin SDK API enablement         | google       | `google.tf`       |
| GCP GA Data API enablement           | google       | `google.tf`       |
| GCP service accounts + keys (Google Groups, GA Data API) | google | `google.tf` |

## What Stays Manual

- **GitHub Actions CI/CD** — workflows in `.github/workflows/`
- **NixOS VPS configuration** — deployed via `just nixos-deploy-*`
- **Secrets** — SOPS-encrypted in `secrets/`, decrypted by sops-nix on VPS
- **GHCR image builds** — handled by GitHub Actions
- **GA4 property access** — the Google provider doesn't manage GA Admin (different API). See "Google Analytics setup" below.

## Google Analytics setup

The `ga-poller` job needs a service account with Viewer access on the GA4 property. Terraform creates the service account and key; granting Viewer on the property is one manual click.

```bash
# 1. Apply — creates the SA, enables the API, generates a key
just tf-apply

# 2. Copy the SA email
terraform -chdir=terraform output -raw ga_data_api_sa_email

# 3. In GA Admin → Property Access Management:
#    Add user → paste the SA email → role: Viewer → Add

# 4. Grab the GA4 numeric Property ID from GA Admin → Property Settings.
#    Format: 9-digit number (NOT the G-XXXXXXXXXX measurement ID).

# 5. Decode the key JSON
terraform -chdir=terraform output -raw ga_data_api_sa_key_json | base64 -d

# 6. Paste both values into the prod and staging web secrets:
sops secrets/prod-web.yaml      # add google_analytics_property_id and google_analytics_service_account_key
sops secrets/staging-web.yaml   # same
```

The private key lives in encrypted S3 backend state and in sops-encrypted YAML — no plaintext on disk.

## File Overview

| File                       | Purpose                                                       |
|----------------------------|---------------------------------------------------------------|
| `main.tf`                  | Terraform block, providers, S3 backend                        |
| `variables.tf`             | All input variables                                           |
| `cloudflare.tf`            | Cloudflare DNS records and proxy settings                     |
| `digitalocean.tf`          | DigitalOcean droplet, firewall, SSH keys                      |
| `secrets.tf`               | SOPS-encrypted secret variable declarations                   |
| `../secrets/terraform.yaml` | SOPS-encrypted secret values (in repo-root `secrets/`)       |
| `google.tf`                | GCP service account for Google Groups API                     |
| `outputs.tf`               | Useful output values                                          |
| `imports.tf`               | Generated by `tf-import.sh`, deleted after apply (gitignored) |
