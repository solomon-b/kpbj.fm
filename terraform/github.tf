# ──────────────────────────────────────────────────────────────
# GitHub — Actions Secrets for kpbj.fm
# ──────────────────────────────────────────────────────────────

locals {
  github_repo = "kpbj.fm"
}

# ──────────────────────────────────────────────────────────────
# Branch Protection — main
# ──────────────────────────────────────────────────────────────

resource "github_repository_ruleset" "main_protection" {
  repository  = local.github_repo
  name        = "main-protection"
  target      = "branch"
  enforcement = "active"

  conditions {
    ref_name {
      include = ["~DEFAULT_BRANCH"]
      exclude = []
    }
  }

  bypass_actors {
    actor_id    = 5 # admin role (repo owner)
    actor_type  = "RepositoryRole"
    bypass_mode = "always"
  }

  rules {
    pull_request {
      required_approving_review_count = 1
    }
  }
}

# ──────────────────────────────────────────────────────────────
# CODEOWNERS
# ──────────────────────────────────────────────────────────────

resource "github_repository_file" "codeowners" {
  repository          = local.github_repo
  branch              = "main"
  file                = ".github/CODEOWNERS"
  content             = "/.github/workflows/ @solomon-b\n"
  commit_message      = "chore: Add CODEOWNERS for workflow files."
  overwrite_on_create = true
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
