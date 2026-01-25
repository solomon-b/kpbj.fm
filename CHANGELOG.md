# Changelog

All notable changes to KPBJ 95.9FM are documented in this file.

## [Unreleased]

### Fixes
- **Events Page Ordering** - Public events page now displays latest events at the top (ordered by start date descending)

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
