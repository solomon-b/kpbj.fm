# ──────────────────────────────────────────────────────────────
# GitHub — Actions Secrets for kpbj.fm
# ──────────────────────────────────────────────────────────────

locals {
  github_repo = "kpbj.fm"
}

# ──────────────────────────────────────────────────────────────
# SSH Known Hosts (pinned host keys for deploy workflows)
# ──────────────────────────────────────────────────────────────

resource "github_actions_secret" "production_known_host" {
  repository      = local.github_repo
  secret_name     = "PRODUCTION_KNOWN_HOST"
  plaintext_value = data.sops_file.secrets.data["production_known_host"]
}

resource "github_actions_secret" "staging_known_host" {
  repository      = local.github_repo
  secret_name     = "STAGING_KNOWN_HOST"
  plaintext_value = data.sops_file.secrets.data["staging_known_host"]
}
