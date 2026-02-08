# ──────────────────────────────────────────────────────────────
# SOPS — In-memory secret decryption
# ──────────────────────────────────────────────────────────────
#
# Decrypts secrets.yaml at plan/apply time. No plaintext secrets
# on disk — the encrypted file is committed to git and decrypted
# in memory by the provider using age keys.
#
# Edit secrets: sops terraform/secrets.yaml
# ──────────────────────────────────────────────────────────────

data "sops_file" "secrets" {
  source_file = "${path.module}/secrets.yaml"
}
