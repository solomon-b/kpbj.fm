# ──────────────────────────────────────────────────────────────
# Google Cloud — Admin SDK service account for Google Groups
# ──────────────────────────────────────────────────────────────

# Enable the Admin SDK API
resource "google_project_service" "admin_sdk" {
  service            = "admin.googleapis.com"
  disable_on_destroy = false
}

# Service account for Google Groups API access
resource "google_service_account" "google_groups" {
  account_id   = "kpbj-google-groups"
  display_name = "KPBJ Google Groups"
  description  = "Service account for managing hosts@kpbj.fm Google Group from the admin dashboard"
}

# Generate a key for the service account
resource "google_service_account_key" "google_groups" {
  service_account_id = google_service_account.google_groups.name
}
