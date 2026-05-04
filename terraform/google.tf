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

# ──────────────────────────────────────────────────────────────
# Google Analytics Data API — service account for ga-poller job
# ──────────────────────────────────────────────────────────────
#
# Powers the daily `ga-poller` systemd job (jobs/ga-poller/),
# which writes top referrers, countries, and cities into the
# ga_snapshots table for the analytics dashboard.
#
# After apply, the SA email must be granted Viewer access on the
# GA4 property — that step is not in the Google provider. See
# README.md ("Google Analytics setup").
# ──────────────────────────────────────────────────────────────

# Enable the GA Data API
resource "google_project_service" "analytics_data" {
  service            = "analyticsdata.googleapis.com"
  disable_on_destroy = false
}

# Service account for GA Data API access
resource "google_service_account" "ga_data_api" {
  account_id   = "kpbj-ga-data-api"
  display_name = "KPBJ GA Data API"
  description  = "Service account for the ga-poller job to read GA4 reports"
}

# Generate a key for the service account
resource "google_service_account_key" "ga_data_api" {
  service_account_id = google_service_account.ga_data_api.name
}
