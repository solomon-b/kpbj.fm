# KPBJ 95.9FM System Architecture

## Overview

```
                                      INTERNET
                                         │
            ┌────────────────────────────┼────────────────────────────┐
            │                            │                            │
            ▼                            ▼                            ▼
     ┌─────────────┐             ┌─────────────┐             ┌─────────────┐
     │  Listeners  │             │   Visitors  │             │    Hosts    │
     │  (Stream)   │             │  (Website)  │             │  (Dashboard)│
     └──────┬──────┘             └──────┬──────┘             └──────┬──────┘
            │                           │ HTTPS                     │ HTTPS
            │ HTTPS                     └─────────────┬─────────────┘
            │                                         │
            ▼                                         ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DIGITALOCEAN VPS                                  │
│                                                                             │
│  ┌──────────────────────────────────────────────────────────────┐           │
│  │                          Nginx                               │           │
│  │  stream.kpbj.fm → Icecast              www.kpbj.fm → Web    │           │
│  └───────┬──────────────────────────────────────────┬───────────┘           │
│          │ HTTP                                     │ HTTP                  │
│          ▼                                          ▼                      │
│  ┌─────────────────┐                       ┌─────────────────┐             │
│  │     Icecast     │                       │   WEB SERVICE   │             │
│  │  • /stream      │                       │  • Haskell      │             │
│  │  • 128kbps MP3  │                       │  • Servant      │             │
│  └────────▲────────┘                       │  • HTMX         │             │
│           │ MP3                             └────────┬────────┘             │
│  ┌────────┴────────┐                                │                      │
│  │    Liquidsoap   │       localhost                 │                      │
│  │  • Polls API ───────────────────────────────────►│                      │
│  │  • Crossfades   │                       ┌────────┴────────┐             │
│  └─────────────────┘                       │   PostgreSQL    │             │
│                                            │  • Users, Shows │             │
│                                            │  • Episodes     │             │
│                                            └─────────────────┘             │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
                                                     │
                                                     │ S3 API
                                                     ▼
                                              ┌───────────┐
                                              │ DO Spaces │
                                              │ • Audio   │
                                              │ • Images  │
                                              └───────────┘
```

## Services

### Web Service

Haskell/Servant application with server-side HTML rendering (Lucid2 + HTMX). Serves the public website, host dashboard, and playout API. Uses PostgreSQL for data and DigitalOcean Spaces for media files.

- **Production**: DigitalOcean VPS (native NixOS systemd service) with local PostgreSQL
- **Staging**: DigitalOcean VPS (native NixOS systemd service) with local PostgreSQL

### Liquidsoap (VPS)

Audio automation daemon. Asks the web service for the next timeline segment before each boundary, plays its items in order, and outputs 128kbps MP3 to Icecast.

### Icecast (VPS)

Streaming server behind NixOS-managed Nginx (automatic Let's Encrypt via ACME). Accepts audio from Liquidsoap and serves up to 100 concurrent listeners on `/stream`.

## Scheduled Jobs

Nine small Haskell programs in `jobs/` do the periodic work. Each one is its own cabal package and its own binary. On the VPS, each job is a systemd `oneshot` service named `kpbj-<job>`, and a systemd timer with the same name starts it. A job starts, does its work, and exits. The diagram above does not show the jobs.

| Job                     | Default schedule   | What it does                                                                                   | Talks to                     |
|-------------------------|--------------------|------------------------------------------------------------------------------------------------|------------------------------|
| `listener-snapshots`    | Every minute       | Reads the listener count from Icecast and adds a row to `listener_snapshots`.                  | Icecast, PostgreSQL          |
| `sync-schedule-calendar` | Every 5 minutes   | Makes the station Google Calendar match the broadcast schedule. See "Schedule calendar sync" below. | PostgreSQL, Google Calendar  |
| `order-cleanup`         | Every 30 minutes   | Cancels pending orders older than 30 minutes and restores their inventory.                     | PostgreSQL                   |
| `token-cleanup`         | Hourly             | Deletes expired email verification and password reset tokens, and all tokens older than 90 days. | PostgreSQL                 |
| `staged-upload-cleanup` | Hourly             | Deletes expired staged uploads: the file first, then the row.                                  | PostgreSQL, storage          |
| `sync-host-emails`      | Hourly             | Makes the members of the hosts Google Group match the hosts in the database.                   | PostgreSQL, Google Groups    |
| `episode-check`         | Daily              | Emails each host whose show airs in 5, 3, or 1 days and has no episode audio.                  | PostgreSQL, SMTP             |
| `ga-poller`             | Daily at 01:30     | Copies the top session sources, countries, and cities of the previous day from Google Analytics into `ga_snapshots`. The first run fills the past 90 days. | PostgreSQL, Google Analytics |
| `mailchimp-reconcile`   | Daily at 02:30     | Compares `newsletter_subscribers` with the Mailchimp audience and repairs the differences.     | PostgreSQL, Mailchimp        |

The schedule of a job is the `timerInterval` option in `nixos/<job>.nix`. The timers use the system time zone of the VPS. `mailchimp-reconcile` also has a `kpbj-mailchimp-reconcile-bootstrap` service with no timer. Start it by hand one time to import the Mailchimp audience into PostgreSQL.

Six jobs have a `--dry-run` flag, which logs what the job would do and changes nothing: `token-cleanup`, `order-cleanup`, `staged-upload-cleanup`, `episode-check`, `sync-host-emails`, and `sync-schedule-calendar`. `nixos/prod.nix` and `nixos/staging.nix` set the flag for each host. Staging runs `sync-host-emails`, `sync-schedule-calendar`, and `episode-check` as dry runs, so it sends no email and changes no Google Group or calendar. `staged-upload-cleanup` is a dry run on both hosts on purpose, so it deletes nothing yet.

### The job monad

Every job runs in `JobM` from the `kpbj-job` library (`lib/kpbj-job/src/Job.hs`). `JobM` holds one database connection for the whole run and has a `MonadDB` instance. A job therefore runs statements with `execQuery`, as the web service does.

`main` of a job reads its own environment variables, then calls `runJob "<job name>"`. `runJob` does the common setup:

1. It reads `DATABASE_URL` and opens the connection.
2. It starts a stdout logger with the job name as the component.
3. It runs the job.
4. It closes the connection.

`runJob` logs at info level and above. `runJobWith` takes the maximum log level as its first argument. A job with a `--verbose` flag passes `Log.LogTrace` there, as `sync-schedule-calendar` does.

HTTP clients, tokens, and configs are not in `JobM`. A job passes them to its functions as arguments.

### Failures and alerts

The log lines of a job go to the systemd journal, and Loki collects them.

- A setup failure, such as a missing environment variable or a refused connection, writes an `ERROR:` line to stderr. The job exits with code 1.
- `execQuery` returns a failed statement as a `Left`. The job logs it at `ATTENTION` level with its own message. Then it stops with code 1 or continues, as that job needs.
- An exception that escapes the job is logged at `ATTENTION` level as "Job failed with an exception". The job exits with code 1.
- `execQuery` also writes one `db query` line at info level for each statement.

The `batch-job-errors` alert in `nixos/monitoring.nix` fires when a job unit logs `ATTENTION`, `ERROR`, or `FATAL` in a 5 minute window. Keep the message of an `ATTENTION` line stable, because the alert and the dashboards read these lines. The alert lists eight jobs by unit name.

`sync-schedule-calendar` has its own alert, `sync-schedule-calendar-errors`. The job runs every 5 minutes, and each run repairs what the run before it did not finish. So the alert fires only when two successive runs log such a line.

### Add a job

1. Create `jobs/<job>/` with a cabal file and `Main.hs`. Depend on `kpbj-job` and `web-server-core`.
2. Add the package to `cabal.project`.
3. Add the package to the overlay and to `packages` in `flake.nix`.
4. Write `nixos/<job>.nix` with the service and the timer, and import it in `nixos/prod.nix` and `nixos/staging.nix`.
5. Add the unit name to the `batch-job-errors` alert in `nixos/monitoring.nix`. If one bad run of the job is not a problem, give the job its own rule with a `forDuration`.

## Playout API

Liquidsoap communicates with the web service via two endpoints. `POST /api/playout/played` is authenticated with a shared secret (`X-Playout-Secret` header). `GET /api/playout/now` is not.

| Endpoint              | Method | Purpose                                  |
|-----------------------|--------|------------------------------------------|
| `/api/playout/now`    | GET    | Get the segment of the timeline          |
| `/api/playout/played` | POST   | Report what is playing                   |

`/now` returns `{"current": ..., "next": ...}`. `current` is one segment of the timeline, and `next` is the whole segment after it, or null. A segment is the span between two boundaries, and its items fill the span with no gaps. Every segment is at least 30 minutes long. The items are an episode, a rerun, a break, ephemeral tracks, or filler. A rerun is an earlier episode of the show, and it airs in a slot that has no new episode. Each item has a planned start time. Each segment has `ends_in_seconds`, the seconds from the request to the end of the segment. It has `id`, a SHA-256 hash of the segment start, the segment end, and the URL, cue points, break item id, and planned start of each item. The id changes only when what airs in the segment changes, and a resumed segment keeps it.

Liquidsoap plays the items one after another and does not use the planned starts. The stored lengths are rounded up to whole seconds, so the items usually end a little before their planned ends. A filler item at the end of a segment therefore has no cue out. It plays until the swap at the boundary stops it, so the gap has audio and not silence. When the items run long, the swap cuts the filler and not a break track. A segment that does not end with filler can still end with a short silence. The filler file must be longer than 180 seconds, so that a trailing filler of the full 180 seconds has spare audio.

While a segment is on the air, Liquidsoap downloads the files of `next` and holds them. A file that does not download does not stop the files after it, and it is tried again every 5 seconds. A file that never downloads is left out of the held segment, and the rest of the segment plays. 10 seconds before the boundary, Liquidsoap asks `/now` for the next segment again. If its `id` matches the id of the held segment, Liquidsoap uses the held files. If not, it downloads the new segment. It swaps to the new segment at the boundary. If that request fails, Liquidsoap plays the held segment. If nothing is held, it asks `/now` again at the boundary. A restart asks `/now` at once, and the answer holds the segment that airs now. So a restart keeps the schedule.

`/played` takes `prPlannedStart`, the planned start time of the item. The web service uses it to ignore a repeated report of the same airing. Liquidsoap does not post filler items to `/played`.

### Known limits

After a restart, the audio runs behind the timeline by the download time of the current segment. The swap at the next boundary then cuts that many seconds from the end of the segment. The end of a segment is its break. The same occurs when the next segment changes after Liquidsoap holds it. Liquidsoap then downloads the new segment at the check, and a download of more than 10 seconds makes the audio late.

An item that still does not download at the check is dropped, and the items after it play early. Liquidsoap logs `[KPBJ] Could not resolve <url>` for each try.

### Release steps for the timeline

The first release of the timeline needs these steps, in this order:

1. Upload a filler file of at least 240 seconds to `audio/filler/kpbj-filler.mp3` in the prod bucket. On staging, `just prod-to-staging-audio` makes a placeholder for it.
2. Deploy.
3. Restart Liquidsoap right after the deploy. The deploy does not restart it, and the old script cannot read the new `/now`.
4. Fill the length of the older ephemeral uploads with `scripts/audio-durations-sql.sh ephemeral_uploads prod`. The usage at the top of the script gives the full commands.

## Environments

| Environment | Web Service             | Stream                                | Now Playing                                    |
|-------------|-------------------------|---------------------------------------|------------------------------------------------|
| Production  | https://www.kpbj.fm     | https://stream.kpbj.fm/stream         | https://www.kpbj.fm/api/stream/now-playing     |
| Staging     | https://staging.kpbj.fm | https://stream.staging.kpbj.fm/stream | https://staging.kpbj.fm/api/stream/now-playing |
| Local       | http://localhost:4000   | http://localhost:8000/stream          | http://localhost:4000/api/stream/now-playing   |

## CI/CD

All builds use Nix. Docker images are pushed to GitHub Container Registry (`ghcr.io/solomon-b/`).

### Web Service

| Workflow                  | Trigger                    | Action                                                              |
|---------------------------|----------------------------|---------------------------------------------------------------------|
| `nix.yaml`                | Push, PR                   | Build with Nix                                                      |
| `create-release-tag.yaml` | Merge release PR           | Create `v*` git tag                                                 |
| `deploy-staging.yaml`     | Push to `main`             | NixOS rebuild to staging VPS                                         |
| `deploy-production.yaml`  | Manual (`gh workflow run`) | NixOS rebuild to production VPS                                      |

### VPS Services (NixOS)

**VPS Configuration**: Droplets run NixOS with native systemd services. Configuration is declarative in `nixos/`:

```
nixos/
  common.nix              # SSH, firewall, base packages
  hardware-digitalocean.nix  # DigitalOcean hardware baseline
  networking-prod.nix     # Production droplet static IP (generated by nixos-setup)
  networking-staging.nix  # Staging droplet static IP (generated by nixos-setup)
  streaming.nix           # Streaming services (Icecast, Liquidsoap, Webhook)
  postgresql.nix          # PostgreSQL database
  web.nix                 # Web service (native systemd)
  nginx.nix               # Reverse proxy + ACME (Let's Encrypt)
  sync-host-emails.nix    # Host email sync job (Google Groups)
  sync-schedule-calendar.nix  # Schedule sync job (Google Calendar)
  token-cleanup.nix       # Expired token cleanup job
  order-cleanup.nix       # Stale pending order cleanup job
  staged-upload-cleanup.nix  # Expired staged upload cleanup job
  episode-check.nix       # Missing episode reminder job
  listener-snapshots.nix  # Icecast listener count job
  ga-poller.nix           # Google Analytics poller job
  mailchimp-reconcile.nix # Mailchimp reconcile job
  monitoring.nix          # Grafana, Loki, and alert rules
  sops.nix                # sops-nix secrets (per-service env files + secret paths)
  prod.nix                # Production host config
  staging.nix             # Staging host config
```

**Local development** uses `docker-compose.yml` at the project root.

Service configs live in their own directories:

```
services/icecast/icecast.xml    # Icecast server config
services/liquidsoap/radio.liq   # Liquidsoap automation script
services/webhook/hooks.yaml     # Webhook definitions
```

Deploy commands: `just staging-nixos-deploy`, `just prod-nixos-deploy`

### Schedule calendar sync

The `sync-schedule-calendar` job (`jobs/sync-schedule-calendar/`) keeps one Google Calendar equal to the broadcast schedule. It runs every 5 minutes on production. On staging it runs as a dry run against the production calendar, and it writes nothing.

How it works:

- Each validity window of a schedule template is one recurring event. A replay is a second event.
- The calendar follows the playout timeline. A show appears when it is active and not deleted, with or without a host.
- The job keeps no state. Each event carries the key of its window in a private property, `kpbjSeries`.
- The job owns the calendar for today and later dates. It deletes an event that a person adds.
- The job does not change what the calendar shows for dates before today.
- Nobody must edit the calendar by hand. Share it with staff and hosts with "See all event details" only. The job does not repair one airing that a person moves or deletes. It logs an attention line for that airing on each run, and a person must correct the airing in Google Calendar.

Setup for a new calendar:

1. Create a service account in the `kpbj-fm` Google Cloud project, with no project role. Create a JSON key for it.
2. Enable the Google Calendar API in that project.
3. Create the calendar in the station Workspace. Share it with the service account email, with the permission "Make changes to events". Give no other account that permission.
4. Add three secrets to `secrets/google.yaml`:
   - `google_calendar_sa_email`: the `client_email` field of the key file.
   - `google_calendar_sa_private_key`: the `private_key` field of the key file, as PEM text.
   - `google_calendar_id`: the Calendar ID from the "Integrate calendar" section of the calendar settings.

The log has the counts of each run only. To see one line for each action, run the job with `--verbose`. To see the raw events of the calendar, run it with `--dump`.

## Secrets Management (SOPS)

All secrets are SOPS-encrypted with [age](https://age-encryption.org/) keys and stored in `secrets/`:

```
secrets/
  terraform.yaml           # Terraform provider tokens (DO, Cloudflare)
  prod-streaming.yaml      # Production: Icecast passwords, playout/webhook secrets
  prod-web.yaml            # Production web: DB password, SMTP, AWS keys
  staging-streaming.yaml   # Staging: Icecast passwords, playout/webhook secrets
  staging-web.yaml         # Staging web: DB password, SMTP, AWS keys
  google.yaml              # Google service account credentials (Groups, Calendar)
  backup.yaml              # Backup/sync credentials (prod + staging)
```

**Encryption keys** are defined in `.sops.yaml`: three developer PC keys plus one VPS host key per environment. VPS host keys are derived from the host's SSH ed25519 key via `ssh-to-age`.

**VPS secrets flow:**
1. SOPS-encrypted YAML is committed to git
2. On `nixos-rebuild boot`, [sops-nix](https://github.com/Mic92/sops-nix) decrypts secrets at boot using the host's SSH key (converted to age)
3. sops-nix renders per-service env files (`kpbj-liquidsoap.env`, `kpbj-webhook.env`, `kpbj-web.env`) and restarts affected services
4. Icecast reads secrets directly from sops secret file paths via its ExecStartPre script

**Production web service** receives secrets via sops-nix, same as staging.

## Infrastructure as Code (Terraform)

Infrastructure provisioning is managed by Terraform in `terraform/`. This covers resource creation — CI/CD workflows remain separate. VPS configuration is managed declaratively via NixOS (`nixos/`).

### What Terraform Manages

| Resource                                         | Provider       | File              |
|--------------------------------------------------|----------------|-------------------|
| DigitalOcean streaming droplets (prod + staging) | `digitalocean` | `digitalocean.tf` |
| DigitalOcean firewalls (prod + staging)          | `digitalocean` | `digitalocean.tf` |
| DigitalOcean SSH keys                            | `digitalocean` | `digitalocean.tf` |
| Cloudflare DNS records                           | `cloudflare`   | `cloudflare.tf`   |
| Cloudflare proxy settings                        | `cloudflare`   | `cloudflare.tf`   |

### What Stays Manual

- GitHub Actions CI/CD (`.github/workflows/`)
- NixOS VPS configuration (`just prod-nixos-deploy` / `just staging-nixos-deploy`)
- Secrets (SOPS-encrypted in `secrets/`, decrypted by sops-nix on VPS)
