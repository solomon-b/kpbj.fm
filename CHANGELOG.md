# Changelog

All notable changes to KPBJ 95.9FM are documented in this file.

## [Unreleased]

### Features
- **Icecast Status Display** - Stream settings page now shows live Icecast server status
- **Stream Metadata Proxy** - Added `/api/stream/metadata` endpoint that proxies Icecast stream metadata for the web player
- **Playout API Metadata** - Playout API responses now include track metadata (title, artist, show info)
- **Local Streaming Stack** - Added Docker Compose setup for Icecast + Liquidsoap streaming infrastructure:
  - `just stream-dev-start/stop/logs/restart/status` commands for local development
  - Liquidsoap polls `/api/playout/now` and `/api/playout/fallback` endpoints
  - Automatic URL rewriting for Docker-to-host communication
  - Stream available at `http://localhost:8000/stream`

### Fixes
- **Create Release Tag Action** - Fixed GitHub Actions workflow for creating release tags

### Chores
- **Fetch Ephemeral Audio Script** - Added utility script to download ephemeral audio files
- **Backfill Episode Durations Script** - Added script to populate missing episode duration values

### Infrastructure
- **Icecast Infrastructure** - Initial Icecast streaming server configuration

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
