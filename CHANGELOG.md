# Changelog

All notable changes to KPBJ 95.9FM are documented in this file.

## [Unreleased]

### Features
- **Ephemeral Upload Descriptions** - Added a required `description` field to ephemeral uploads (min 80 characters). Existing uploads are backfilled with their title. Both new and edit forms include an editorial guidelines notice reminding submitters that ephemeral clips speak as the voice of the station. Description is shown as a truncated secondary line in the uploads list.
- **Ephemeral Upload Flagging** - Staff and admins can flag ephemeral uploads with a reason (inappropriate content, poor audio quality, copyright concern). Flagged uploads are dimmed with a visible `[FLAGGED: reason]` label in the dashboard, sorted to the top of the list for staff visibility. Regular users and the Liquidsoap fallback playback endpoint no longer see flagged uploads. Staff can unflag uploads to restore them. New `flag_reason` enum, `flagged_at`/`flagged_by`/`flag_reason` columns, and a partial index on unflagged uploads.
- **Host Email Sync Job** - New standalone Haskell job (`sync-host-emails`) that syncs host email addresses to a Google Group via the Google Admin SDK. Runs as a NixOS systemd timer on the VPS. Google service account credentials managed via SOPS. Terraform provisions the Google project and service account.

### Fixes
- **Icecast Runtime Directory Permissions** - Fixed systemd `RuntimeDirectory` permissions for the Icecast service.
- **CI SSH Key Consolidation** - Consolidated staging and production CI deployments to use a single SSH key.
- **Webhook Endpoint Security** - Removed public-facing webhook endpoint from nginx configuration.
- **Liquidsoap Local Icecast Connection** - Liquidsoap now connects to Icecast via localhost instead of routing through the public network.
- **Stream Subdomain Proxy** - Added nginx proxy rule to route the `stream` subdomain to the Icecast server.

### Infrastructure
- **Rebuilt Staging Droplet** - Rebuilt staging DigitalOcean droplet with updated networking configuration and SOPS keys.
- **Service Directory Reorganization** - Moved Icecast, Liquidsoap, and webhook configs to top-level service directories. Added root `docker-compose.yml` for local development.
- **Staging Web Service VPS Migration** - Migrated staging web service from Fly.io to NixOS-managed DigitalOcean VPS. PostgreSQL managed via NixOS module with automated backups. CI deploys via `nixos-rebuild switch --target-host`. Removed `fly.staging.toml`.
- **Production Web Service VPS Migration** - Migrated production web service from Fly.io to NixOS-managed DigitalOcean VPS. Removed `fly.toml`.
- **Stable Streaming During Deploys** - Added `restartIfChanged = false` to Icecast and Liquidsoap systemd services to prevent stream interruptions during NixOS updates.
- **Faster NixOS Deploys** - Changed CI from `nixos-rebuild boot` + reboot to `nixos-rebuild switch` for immediate rollouts.
- **Stream Config in NixOS** - Moved stream URL and path configuration from the `stream_settings` database table into NixOS infrastructure config (`StreamConfig` in `CustomContext`). Dropped `stream_settings` table via migration. Removed stream settings edit handler; dashboard now shows read-only values from app config.

---

## [0.8.1] - 2026-02-11

### Features
- **Featured Event on Homepage** - Staff/admins can promote a single event's flyer to the homepage, replacing the default range map image. The flyer links to the event detail page via HTMX navigation. When no event is featured (or the featured event has no poster), the default FCC range map is shown. A "Promote"/"Demote" action in the events dashboard dropdown toggles the featured status, automatically clearing any previously featured event. A partial unique index enforces that at most one event can be featured at a time.

### Chores
  - **Add description to episode detail page** - Episode descriptions were missing from the template.
  - **Dashboard design tuneups** - Various small adjustments to the dashboard design and layouts.

---

## [0.8.0] - 2026-02-11

### Features
- **Episode Card Scrub Bar** - Added a progress/seek bar to episode card artwork. A thin bar at the bottom of the artwork image shows playback progress and supports click-to-seek and drag-to-scrub (mouse and touch). Clicking the bar while not playing starts playback at that position. Progress syncs with the persistent navbar player via `requestAnimationFrame`.
- **Missing Episodes Dashboard** - Added admin page at `/dashboard/missing-episodes` showing shows scheduled within the next 7 days that are missing episode uploads (no episode or episode without audio). Table displays show title, hosts, scheduled date, day of week, and time slot, sorted by soonest deadline first. Accessible to staff and admin roles via the sidebar.
- **Liquidsoap Audio Processing** - Added normalization, compression, and limiting to the Liquidsoap broadcast chain. Normalizes volume across tracks (±6 dB range), compresses dynamic range (3:1 ratio, -10 dB threshold), and hard-limits at -1 dB to prevent clipping.

### Improvements
- **Scrollable Playback History** - Playback history table on the stream settings dashboard is now capped at ~15 visible rows with a scrollable container and sticky column headers.

### Fixes
- **Dashboard Nav Links Disabled on Non-Show Pages** - Fixed EPISODES and SHOW SETTINGS sidebar links being disabled when viewing STREAM or SITE PAGES. These handlers now fall back to the first available show so show-scoped nav links remain clickable.
- **Icecast Stream Choppy Audio on Refresh** - Fixed audio jumping between two buffers when refreshing the page during live stream playback. Added `beforeunload` handler to tear down the audio element before page unload, cache-busting on the stream URL to prevent stale connection reuse, and halved Icecast `burst-size` from 65536 to 32768 to reduce audio overlap on reconnect.

### Chores
- **SOPS-Managed Backup & Sync Credentials** - All backup and data sync scripts (`prod-backup-pg`, `prod-backup-s3`, `prod-to-staging-*`, `prod-to-local-*`) now load credentials from SOPS-encrypted `secrets/backup.yaml` instead of requiring env vars in `.envrc.local`. Added `load_secret` helper to `scripts/lib/common.sh`, new `just sops-edit-backup` command, and `prod-backup-s3.sh` falls back to env vars for TrueNAS cron contexts. Renamed `prod-backup` to `prod-backup-pg`.
- **SOPS-Managed Terraform State Backend Credentials** - `just tf-init` now loads state backend S3 keys from SOPS-encrypted `secrets/terraform.yaml` instead of requiring `TERRAFORM_ACCESS_KEY_ID` / `TERRAFORM_SECRET_ACCESS_KEY` env vars. Renamed `tf-edit-secrets` to `sops-edit-terraform` and grouped all `sops-edit-*` commands under a unified "SOPS Secrets" section.
- **Remove Deprecated Stream Deploy Commands** - Removed `just stream-staging-deploy` and `just stream-prod-deploy` Justfile recipes, replaced by NixOS deployment (`just nixos-deploy-staging` / `just nixos-deploy-prod`).

### Infrastructure
- **Terraform + SOPS Infrastructure** - Codified DigitalOcean streaming VPS (separate prod + staging droplets, firewalls, SSH keys) and Cloudflare DNS + proxy settings as Terraform. Secrets managed via SOPS + age encryption. Remote state in Tigris S3.
- **NixOS Streaming VPS** - Replaced Ubuntu/Docker Compose/manual nginx+certbot setup with fully declarative NixOS configuration. Droplets are provisioned via nixos-infect in Terraform user_data and configured with NixOS-managed nginx with automatic ACME/Let's Encrypt TLS and systemd service management. Deploy with `just nixos-deploy-staging` / `just nixos-deploy-prod` via `nixos-rebuild --target-host`.
- **Native Systemd Streaming Services** - Replaced Podman OCI containers with native systemd services for Icecast, Liquidsoap, and Webhook. Removes the container build/push/pull pipeline entirely — packages come directly from nixpkgs. Icecast and Liquidsoap run as `DynamicUser` with systemd hardening; Webhook runs as a static `kpbj-webhook` user with narrow NOPASSWD sudoers rules for restarting the other two services. Per-service sops-nix env files replace the single shared env template. Removed `stream-images.yml` CI workflow, `stream-dev-build`/`stream-dev-rebuild`/`stream-publish` Justfile recipes, and `icecast-docker`/`liquidsoap-docker`/`webhook-docker` Nix packages.
- **sops-nix for Streaming Secrets** - Streaming secrets (Icecast passwords, `PLAYOUT_SECRET`, `WEBHOOK_SECRET`) are now SOPS-encrypted in the repo under `secrets/` and decrypted on VPS at deploy time via sops-nix using the host's SSH key. Replaces per-VPS random secret generation. New Justfile commands (`just fly-sync-secrets-prod`, `just fly-sync-secrets-staging`) sync the same secrets to Fly.io. Consolidated all SOPS files into `secrets/` directory.
- **Consolidate Icecast source password** - Icecast entrypoint now reads `ICECAST_PASSWORD` for `<source-password>` (matching Liquidsoap), eliminating the separate `ICECAST_SOURCE_PASSWORD` variable.
- **Per-environment networking configs** - `nixos-setup` now captures nixos-infect's static IP networking config as `networking-staging.nix` / `networking-prod.nix`, imported by each environment's NixOS config. Replaces DHCP (which doesn't work reliably on DigitalOcean after nixos-infect).
- **Safe NixOS deploys** - `nixos-rebuild boot` + reboot instead of `nixos-rebuild switch` to avoid hangs on first deploy to freshly-infected hosts.

---

## [0.7.0] - 2026-02-09

### Features
- **Stream Status Indicator** - Green/red dot on the stream settings dashboard showing whether Icecast is reachable, with three states: offline (red), online but no source (green), and online with full stats (green)
- **Metadata Persistence on Reconnect** - Liquidsoap now re-sends track metadata to Icecast after reconnection, so stream title/artist survive Icecast container restarts
- **Icecast Healthcheck** - Added Docker healthcheck to Icecast container and `service_healthy` dependency for Liquidsoap, preventing connection failures during startup
- **Container Management Dashboard** - Admins can restart Icecast and Liquidsoap containers directly from the stream settings dashboard via webhook triggers, with confirmation dialogs and status banners
- **Nix-Built Webhook Image** - Replaced `almir/webhook:latest` Alpine image with a Nix-built Docker image (`ghcr.io/solomon-b/kpbj-webhook`) bundling `webhook` and `docker-client`, fixing the glibc incompatibility when mounting the host Docker binary
- **Mailing List Signup** - Added newsletter signup form to the About Us page
- **Playback History Logging** - Added `POST /api/playout/played` endpoint for Liquidsoap to report played tracks
- **Icecast Status Display** - Stream settings page now shows live Icecast server status
- **Stream Metadata Proxy** - Added `/api/stream/metadata` endpoint that proxies Icecast stream metadata for the web player
- **Playout API Metadata** - Playout API responses now include track metadata (title, artist, show info)
- **Full Database Query Test Coverage** - Added comprehensive test suites for all database table modules with property-based generators, covering CRUD operations, edge cases, and constraint validation
- **Local Streaming Stack** - Added Docker Compose setup for Icecast + Liquidsoap streaming infrastructure:
  - `just stream-dev-start/stop/logs/restart/status` commands for local development
  - Liquidsoap polls `/api/playout/now` and `/api/playout/fallback` endpoints
  - Automatic URL rewriting for Docker-to-host communication
  - Stream available at `http://localhost:8000/stream`

### Refactoring
- **Convert Queries to Rel8** - Migrated all database queries from `hasql-interpolate` to `rel8`, a type-safe relational algebra library, across all table modules (~2k lines changed across 56 files)

### Fixes
- **Audio Player Preserved on Back/Forward** - Browser history navigation no longer interrupts audio playback. Added `hx-history-elt` to scope HTMX history snapshots to `#main-content`, preserving the player's Alpine state and audio element.
- **Staged Audio Upload Size Limit** - Increased max file size for episode and ephemeral audio uploads from 50MB to 500MB
- **Banner Dismiss on Click Away** - Banners now dismiss when user clicks outside of them
- **Episode Metadata Escaping** - Fixed HTML escaping in episode metadata to prevent XSS
- **Form Builder Progress Bar** - Progress bar now only shows on forms with file upload fields
- **Create Release Tag Action** - Fixed GitHub Actions workflow for creating release tags
- **Duplicate cabal.project** - Removed duplicate `cabal.project` file

### Infrastructure
- **Webhook CI Pipeline** - Added webhook image to the stream images build-and-publish GitHub Actions workflow
- **Docker Compose Ports** - Assigned explicit ports in environment-specific compose files
- **Stream CI Pipeline** - Added GitHub Actions workflow for building and publishing stream container images
- **LiquidSoap Secrets** - Added secret generation for LiquidSoap service authentication
- **Icecast Infrastructure** - Initial Icecast streaming server configuration

### Chores
- **Two-Phase CustomContext Initialization** - Refactored `CustomContext` initialization to separate config loading from resource building, with structured JSON logging
- **StorageContext Uses Environment** - StorageContext now respects the Environment setting for storage backend selection
- **Script Library Refactor** - Extracted shared constants and functions into `scripts/lib/common.sh` (database ports, bucket names, sanitized password hash) and centralized PII sanitization into `scripts/lib/sanitize-pii.sql`. Refactored all prod-to-local and prod-to-staging scripts to use the common library.
- **Incremental Prod-to-Staging Sync** - Prod-to-staging S3 sync is now incremental via a shared `scripts/lib/sync-s3.sh` library, avoiding full re-uploads on every sync
- **Remote Backup Commands** - Added `just prod-backup-remote` and `just prod-backup-s3` for automated backups from external servers (e.g., TrueNAS cron jobs)
- **Justfile Cleanup** - Removed unused test-postgres commands, staging mock data commands, and duplicate constants. Staging now uses production data sync instead of mock data.
- **Stream Container Commands** - Added `just stream-rebuild` and `just stream-reload` commands
- **Fetch Ephemeral Audio Script** - Added utility script to download ephemeral audio files
- **Backfill Episode Durations Script** - Added script to populate missing episode duration values
- **Add weeder.toml to Root** - Added Weeder configuration file to the project root for dead code detection

### Documentation
- **ARCHITECTURE.md** - New system architecture doc covering all three services (Web, Liquidsoap, Icecast), deployment topology, environments, playout API, and CI/CD pipelines
- **README.md** - Rewrote with service overview, dev setup, environment variables, release process, and streaming deployment
- **CLAUDE.md** - Updated to reference new docs, fixed incorrect PostgreSQL port and removed non-existent deploy command
- **Storage comment fix** - Fixed misleading comment in `App/Storage.hs` about S3 fallback behavior

---

## [0.6.0] - 2026-02-01

### Features
- **Liquidsoap Playout API** - Added two JSON API endpoints for Liquidsoap integration:
  - `GET /api/playout/now` - Returns the currently scheduled episode's audio URL based on schedule templates
  - `GET /api/playout/fallback` - Returns a random ephemeral track URL for fallback playback
  - Both endpoints are public (no auth) and return `{"url": "..."}` or `null` with graceful error handling
- **Ephemeral Upload Edit Form** - Staff and admins can now edit ephemeral uploads (title and audio file) via the dashboard
- **Ephemeral Upload Download Button** - Added download button to ephemeral uploads table for easy file retrieval
- **Ephemeral Upload Filenames** - Ephemeral audio files now use the slugified title in the filename (e.g., `weather-report_2026-02-01_abc123.mp3`) instead of generic `ephemeral_` prefix

### Fixes
- **Episode Image Filepath Date Format** - Fixed date format in episode image file paths for consistent organization

### Chores
- **Monorepo Restructure** - Reorganized project into `services/` directory structure to support multiple services. Web application moved to `services/web/`, with placeholder for upcoming LiquidSoap streaming service at `services/liquidsoap/`. Root `cabal.project` now references all packages. Updated `flake.nix`, `Justfile`, CI workflows, and scripts with new paths.

---

## [0.5.1] - 2026-02-01

### Features
- **Stream Path Configuration** - Dashboard now allows setting the stream path for shows
- **Pacific Time Show Storage** - Shows are now stored in Pacific time for consistent local scheduling

### Fixes
- **Episode Form Time Localization** - Fixed time handling in episode forms to use correct timezone
- **Event DateTime UTC** - Event datetimes are now recorded in UTC for consistency
- **Episode Edit Cancel Redirect** - Fixed cancel button on episode edit form redirecting to wrong page

### Chores
- **Weeder Disabled** - Temporarily disabled Weeder dead code detection

---

## [0.5.0] - 2026-01-31

### Features
- **Show Title on Schedule** - Schedule page now displays show title under the show image for better identification
- **12 Hour Clock** - Render times with a 12 hour clock.
- **Simplified Event Details** - Removed back button and author section from event detail pages for cleaner presentation

### Fixes
- **Dark Mode Theme Colors** - Fixed ~30 files with hardcoded Tailwind colors (bg-gray-*, text-gray-*, border-gray-*) that caused white-on-white text and poor contrast in dark mode. All colors now use dynamic theme tokens from `Design.Tokens` that respect the user's theme preference.

### Chores
- **Weeder in CI** - Added dead code detection via Weeder to the CI pipeline
- **Design Tokens Cleanup** - Consolidated and cleaned up design token definitions across 64 files for better consistency
- **Factor out lucid-tailwind** - Extracted Tailwind CSS combinators (`cls`, `clsWhen`, breakpoint prefixes, state variants, grid utilities) into a standalone `lucid-tailwind` package for reuse across projects
- **Dead Code Cleanup** - Removed unused functions: `utcToPacificDay`, `EmailVerificationTokens.getByToken`, `EmailVerificationTokens.getLatestPendingForUser`, sync email functions (`sendEmail`, `sendVerificationEmail`, `sendPasswordResetEmail`, `sendHostAssignmentEmail`), `buildSimpleMail`, `claimStagedUploadMaybe`
- **Schedule Shows Filter** - Public schedule page now only displays shows that have assigned hosts
- **Remove OpenTelemetry** - Removed hs-opentelemetry dependency to simplify the codebase
- **Upstream Cookie Utils** - Migrated to upstream web-server-core cookie utilities, removing local implementations

### Fixes
- **Markdown Line Breaks** - Enabled hard line breaks in markdown rendering so single newlines render as `<br>` tags instead of being collapsed into spaces

---

## [0.4.2] - 2026-01-29

### Fixes
- **Stream Player Now Playing** - Fixed stream player to show current track/DJ info from AzuraCast instead of hardcoded "KPBJ 95.9 FM". Now fetches from the AzuraCast Now Playing API and displays live DJ name or "Artist - Title" for playlist tracks
- **Infinite Scroll Grid Layout** - Fixed blank spots appearing in grid layouts when infinite scroll loads new content. Sentinel and end-of-content elements now span full grid width

---

## [0.4.1] - 2026-01-29

### Features
- **Schedule Page Note** - Added informational note explaining that outside scheduled programming, KPBJ plays curated ephemeral content from hosts and DJs
- **Schedule Page Dates** - Schedule page now shows the date (e.g., "Wednesday, Jan 28") in addition to the day of week

### Chores
- **Prod to Local Sync** - Added `just prod-to-local`, `just prod-to-local-db`, and `just prod-to-local-files` commands to copy production data to local development with PII sanitization

### Fixes
- **Episode Date Display Timezone** - Fixed episode scheduled dates displaying one day ahead (e.g., Feb 4th instead of Feb 3rd) for evening shows. The `scheduledAt` UTC timestamp is now properly converted to Pacific time before formatting. Added `tz` library dependency for DST-aware `America/Los_Angeles` timezone handling
- **Schedule Timezone Bug** - Fixed schedule template creation/editing using UTC instead of Pacific time, which caused an ~8 hour window where old and new templates could overlap incorrectly after editing
- **Staging Login Cookie Collision** - Fixed issue where production cookies (Domain=.kpbj.fm) were sent to staging (staging.kpbj.fm) causing login failures. Each environment now uses a unique cookie name: `session-id-staging` and `session-id-production`
- **File Upload MIME Types** - Fixed file uploads being rejected due to MIME type charset suffixes (e.g., `audio/mpeg; charset=binary` was not matching `audio/mpeg`)
- **Station ID Playback on S3** - Fixed station ID audio files not playing on staging/production. The template was hardcoding `/media/` URLs instead of using `buildMediaUrl` which generates proper S3 URLs in production

---

## [0.4.0] - 2026-01-28

### Features
- **New Themes** - Added Gruvbox, Dracula, and Nord color themes with light and dark variants
- **AM/PM Show Replays** - Shows can now be scheduled as AM or PM replays of other shows
- **Audio Preview in Upload Forms** - Episode upload form now includes an inline audio player to preview files before submitting
- **Improved Upload UX** - File uploads now show "Processing..." status after transfer completes while server processes the file, with animated progress bar and disabled submit button to prevent premature form submission
- **Cross-Subdomain Authentication** - Session cookies now set domain attribute for seamless authentication across subdomains

### Chores
- **Factor out Lucid Libraries** - Pulls out lucid-form-builder and lucid-htmx-alpine as standalone packages
- **Theme Storage** - Changed theme preference storage from PostgreSQL enum to text for easier extensibility
- **Prod to Staging Copy** - Added `just prod-to-staging`, `just prod-to-staging-db`, and `just prod-to-staging-s3` commands to copy production data to staging with PII sanitization for User/Host accounts
- **Increased Upload Limit** - Fly.io max request body size increased to support larger audio file uploads

### Fixes
- **Events Page Ordering** - Public events page now displays latest events at the top (ordered by start date descending)
- **Markdown Theme Colors** - Markdown content now properly uses theme colors instead of hardcoded values
- **Audio Upload CORS** - Fixed CORS errors preventing audio file uploads from completing
- **Host Profile Schedule** - Fixed schedule display on host profile pages

---

## [0.3.4] - 2026-01-25

### Features
- **Dynamic Pages** - Added support for dynamic CMS-style pages
- **Theme System** - New theme system for customizable site styling
- **Soft Delete for Shows** - Admins can now delete shows via the dashboard (soft delete preserves data while hiding from all queries)
- **Rolling Schedule View** - Schedule page now shows today + next 6 days instead of the current Mon-Sun week
- **Optional Show Descriptions** - Show descriptions are now optional when creating or editing shows

### Fixes
- **Verification Sent Template** - Fixed verification sent email template
- **User Delete Permissions** - User delete button in dashboard is now only visible to admins, and admins cannot delete themselves

### Chores
- **Event Poster Ratio** - Changed event poster image aspect ratio from 9:16 to 3:4
- **Embedded Static Assets** - Static files (range.png) are now embedded at compile time using `file-embed`, removing the need for filesystem-based static file serving
- **Remove Banner Images** - Removed banner images from the codebase
- **Remove APP_ENVIRONMENT** - Removed APP_ENVIRONMENT configuration variable
- **Remove Primary Host Concept** - Schedule page now shows all hosts for a show instead of just a "primary" host

---

## [0.3.3] - 2026-01-22

### Features
- **Public Events Page** - Re-enabled the Events page in the public site navigation (desktop and mobile)
- **Ephemeral Uploads** - Hosts can upload random audio clips for nighttime playback, with full CRUD operations and inline audio preview
- **Auto-Login After Email Verification** - Users are now automatically logged in after verifying their email address, eliminating the need to enter credentials again
- **Host Assignment Emails** - Automatic email notifications when hosts are assigned to shows, with show name and timeslot information
- **Optional Show Descriptions** - Show descriptions are now optional when creating/editing shows
- **Background Cleanup Jobs** - Automated cleanup of expired staged uploads and other maintenance tasks
- **Email Rate Limiting** - Verification emails are now rate-limited to prevent abuse (60-second cooldown)
- **Manual Deploy Trigger** - Production deployments can now be triggered manually via GitHub Actions
- **Release Notes Integration** - `just release-pr` now auto-generates release notes from CHANGELOG.md

### Fixes
- **Dashboard Dark Mode** - Fixed dark/light mode toggle not working in dashboard (was always automatic mode)
- **Ephemeral Upload File Cleanup** - Deleting an ephemeral upload now removes the audio file from storage (previously only deleted the database record)
- Fixed dashboard authentication requirements for admin routes
- **Role-Aware Auth Redirects** - Dashboard authorization failures now redirect based on user role (dashboard users stay in dashboard, public users go to home)
- Fixed user avatar display on user detail page (now renders image in header instead of raw URL)
- Fixed TBD display for shows without scheduled timeslots
- Fixed show edit form cancel button redirect
- Fixed display of hosts without host_details records
- Fixed staff permission check for creating shows
- Fixed safe link usage in email verification templates

### Chores
- **Event Poster Ratio** - Changed event poster image aspect ratio from 2:3 to 9:16 to match Instagram Stories format
- Disabled signup link on login form (invite-only registration)
- Added CHANGELOG.md for tracking releases
- **MP3-Only Audio Uploads** - Restricted audio uploads to MP3 format only (client and server-side validation)
- **Standardized Email Styling** - All emails now use plain text with consistent 90s monospace ASCII art styling

---

## [0.3.2] - 2025-01

### Features
- **Password Reset via Email** - Users can now reset their passwords through email
- **Email Verification** - New user accounts require email verification
- **Version Route** - Added `/version` endpoint for deployment verification
- **Dark Mode** - Fixed and improved dark mode support
- **Station ID Uploads** - Hosts can now upload station identification audio

### Infrastructure
- New release system with GitHub Actions
- Pinned dependencies to specific commits for reproducibility

---

## [0.3.1] - 2025-01

### Features
- **S3/Object Storage Support** - Production file storage via Tigris (S3-compatible)
- **Staged Uploads** - Two-phase commit pattern for reliable file uploads
- **Google Analytics** - Analytics integration for production environment
- **Concrete Application Monad** - Migrated from polymorphic MTL-style to concrete `AppM` monad for faster compile times

### Fixes
- Fixed image form fields to allow clearing uploaded images
- Fixed empty episode audio uploads
- Fixed episode upload form field names
- Fixed dashboard display when user has no shows
- Fixed newsletter signup

### Infrastructure
- Added production environment configuration
- Added `/tmp` directory in Docker container for uploads

---

## [0.3.0] - 2024-12

### Features
- **Dashboard Show Settings** - Comprehensive show management page
- **Improved Track Form UX** - Better interface for adding track listings to episodes
- **Episode Tags** - Tags now displayed on dashboard episode pages
- **Single Day Schedule View** - Schedule page shows single day at a time
- **Standardized HTMX Patterns** - Consistent response patterns across all handlers
- **Form Builder V2** - New form builder with password fields and improved UX
- **Exception-Based Error Handling** - Cleaner error handling throughout the application

### Fixes
- Fixed draft episodes appearing on public site
- Fixed dashboard sidebar highlighting
- Fixed donation page mobile rendering
- Fixed logout redirects
- Standardized tag rendering across the site

### Refactoring
- Moved episode edit handlers under dashboard namespace
- Reorganized dashboard templates
- Migrated auth forms to FormBuilder
- Moved show blog handlers to dashboard

---

## [0.2.x] - 2024-11/12

### Major Features
- **Episode Management** - Full episode upload, edit, and publishing workflow
- **Show Management** - Create and manage radio shows with schedules
- **Host Dashboard** - Dedicated dashboard for show hosts
- **Blog System** - Dual blog system (station blog + show-specific blogs)
- **Events Calendar** - Community events with calendar views
- **User Authentication** - Cookie-based sessions with role hierarchy
- **HTMX Integration** - HTML-over-the-wire architecture with HTMX

### Infrastructure
- Servant-based REST API with type-safe routing
- PostgreSQL database with Hasql
- Lucid2 for server-side HTML rendering
- Docker deployment on Fly.io
- Nix Flakes for reproducible builds

---

## Version History

| Version | Date | Highlights |
|---------|------|------------|
| 0.3.2 | Jan 2025 | Password reset, email verification, station IDs |
| 0.3.1 | Jan 2025 | S3 storage, staged uploads, concrete monad |
| 0.3.0 | Dec 2024 | Dashboard improvements, form builder v2 |
| 0.2.x | Nov-Dec 2024 | Core platform features |
