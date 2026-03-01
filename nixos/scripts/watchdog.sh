# shellcheck shell=bash
# ──────────────────────────────────────────────────────────────
# KPBJ Watchdog — LLM-powered log anomaly detection
# ──────────────────────────────────────────────────────────────
#
# Collects journal logs, service statuses, and system stats from
# a NixOS droplet, sends them to the Gemini 2.5 Flash API for
# anomaly detection, and emails the operator via msmtp if
# anything looks wrong.
#
# Expected to be wrapped by pkgs.writeShellApplication which
# adds the shebang and set -euo pipefail. Do NOT add a shebang.
# ──────────────────────────────────────────────────────────────

# ── 1. Validate required environment variables ───────────────

required_vars=(
  GEMINI_API_KEY
  WATCHDOG_ENV
  WATCHDOG_RECIPIENT
  WATCHDOG_INTERVAL
  MSMTP_CONFIG
)

for var in "${required_vars[@]}"; do
  if [[ -z "${!var:-}" ]]; then
    echo "ERROR: Required environment variable ${var} is not set." >&2
    exit 1
  fi
done

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

echo "[${TIMESTAMP}] Watchdog starting (env=${WATCHDOG_ENV}, interval=${WATCHDOG_INTERVAL}m)"

# ── 2. Collect data ──────────────────────────────────────────

# Journal logs from KPBJ services and PostgreSQL.
# journalctl may return non-zero if no entries match the time window.
journal_logs=$(journalctl \
  --since "${WATCHDOG_INTERVAL} minutes ago" \
  -u 'kpbj-*' \
  -u postgresql \
  --no-pager \
  --no-hostname \
  -n 500 2>&1) || true

# Service statuses. systemctl status returns non-zero for
# failed/inactive services, which is expected.
service_status=$(systemctl status \
  kpbj-web \
  kpbj-icecast \
  kpbj-liquidsoap \
  kpbj-webhook \
  postgresql 2>&1) || true

# System resource stats.
disk_usage=$(df -h /)
memory_usage=$(free -h)
system_uptime=$(uptime)

# ── 3. Build the Gemini system prompt ────────────────────────

read -r -d '' SYSTEM_PROMPT << 'SYSPROMPT' || true
You are a watchdog for KPBJ 95.9FM community radio infrastructure running on a NixOS server. Your job is to analyze system logs, service statuses, and resource metrics to detect anomalies that need operator attention.

## KPBJ Services

- **kpbj-web.service** — Haskell/Servant web application. Serves the website, dashboard, and playout API. Should be active and running. Logs HTTP requests and application events.
- **kpbj-icecast.service** — Icecast streaming server. Serves audio to listeners. Should be active and running. Listener connects/disconnects are normal.
- **kpbj-liquidsoap.service** — Liquidsoap audio automation. Polls the web API for the playlist schedule, fetches audio files, and streams to Icecast. Should be active and running. Routine polling and track changes are normal.
- **kpbj-webhook.service** — Webhook listener for service restarts triggered by deployments. Should be active and running. Occasional restart commands are normal during deploys.
- **postgresql.service** — PostgreSQL database. Should be active and running. Routine autovacuum, checkpoints, and connection handling are normal.
- **kpbj-token-cleanup.service** — Oneshot timer job. Deletes expired auth tokens. Runs hourly. It is normal for this to be inactive (dead) between runs.
- **kpbj-sync-host-emails.service** — Oneshot timer job. Syncs host emails from Google Workspace. Runs hourly. It is normal for this to be inactive (dead) between runs.
- **kpbj-backup-full.service** — Oneshot timer job. Runs pgBackRest full PostgreSQL backup. Runs daily. It is normal for this to be inactive (dead) between runs.

## What to FLAG (anomalies requiring attention)

- Any long-running service (kpbj-web, kpbj-icecast, kpbj-liquidsoap, kpbj-webhook, postgresql) in failed, inactive, or activating state
- Repeated crash loops or rapid restart patterns
- Error spikes: many ERROR or FATAL log lines in a short period
- Out-of-memory (OOM) kills or memory pressure warnings
- Disk usage above 85%
- Unusual SSH login patterns (failed auth from unexpected IPs, brute force)
- Liquidsoap failing to fetch audio or losing connection to Icecast
- Icecast mount point going down or source disconnecting unexpectedly
- Database errors: connection refused, too many connections, replication lag, corruption
- Backup failures in kpbj-backup-full
- TLS/certificate errors
- Systemd dependency failures (service failed because a required unit failed)

## What is NORMAL and should NOT be flagged

- Listener connects and disconnects in Icecast (these are routine)
- Liquidsoap polling the API and fetching tracks (routine operation)
- Liquidsoap "Ffmpeg_decoder.End_of_file" messages — this is normal track completion, not a decoding failure. FFmpeg raises End_of_file when it finishes reading a track. The subsequent "Finished with" and new request preparation confirm healthy track transitions.
- Liquidsoap "Could not update timestamps for discarded samples" warnings — common with VBR MP3s, does not affect playback
- Liquidsoap/FFmpeg "Header missing" warnings — FFmpeg's mp3float decoder encountered a missing or corrupted MP3 frame header mid-file. This is a non-fatal warning; the decoder skips the bad frame and continues. Common with old or non-standard MP3 encodings. Does not affect playback.
- Liquidsoap ID3 tag parsing warnings: "Unsynchronized headers not handled", "Incorrect BOM value", "Error reading comment frame, skipped" — these mean Liquidsoap's built-in ID3 parser can't read certain tag formats. FFmpeg handles them fine as fallback. Does not affect playback.
- Liquidsoap "Unsupported MIME type" or "Unsupported file extension" messages during decoder selection — this is normal decoder negotiation, not an error. Liquidsoap tries each decoder in priority order and the correct one (usually ffmpeg) is selected.
- MP3 files reported as containing video streams (e.g. "video: {codec: mjpeg, ...}") — this is embedded album art in the MP3, not actual video. Completely normal.
- PostgreSQL autovacuum, checkpoint, and WAL archiving activity
- Oneshot timer services (kpbj-token-cleanup, kpbj-sync-host-emails, kpbj-backup-full) showing as inactive/dead — they only run on their timer schedule
- Empty or sparse journal logs during quiet periods
- Normal HTTP request logs (200/301/302 responses)
- Routine service startup messages after a deploy
- pgBackRest info-level backup logs
- Bot scanners probing for .env, .git/config, wp-admin, etc. (androxgh0st and similar) — normal internet noise, ignore unless volume spikes dramatically
- Node.js command injection probes (child_process.execSync, process.mainModule, etc.) in request logs — irrelevant to Haskell/Servant stack, ignore

## Response Format

If everything looks normal, respond with exactly:
NO_ISSUES

If you detect an anomaly, respond with:
SUBJECT: <one-line summary of the most important finding>
<detailed analysis in plain text, explaining what you found, why it matters, and suggested next steps>

Important rules:
- Be concise. Operators are busy.
- Only flag genuine problems, not routine operations.
- If in doubt, do NOT flag it. False alarms erode trust.
- The SUBJECT line must be short enough for an email subject (under 80 chars).
- Never include markdown formatting — this goes into a plain-text email.
SYSPROMPT

# Append known issues to the system prompt if the file is non-empty.
if [[ -s "${WATCHDOG_KNOWN_ISSUES_FILE:-}" ]]; then
  known_issues=$(cat "${WATCHDOG_KNOWN_ISSUES_FILE}")
  SYSTEM_PROMPT="${SYSTEM_PROMPT}

## Known Issues — Do NOT Flag

The following are known, acknowledged issues. Do NOT flag them unless the pattern changes significantly (e.g. volume spikes, new IPs, or different behavior).

${known_issues}"
fi

# ── 4. Build JSON payload with jq (safe escaping) ───────────

user_message=$(jq -n \
  --arg env "${WATCHDOG_ENV}" \
  --arg interval "${WATCHDOG_INTERVAL}" \
  --arg timestamp "${TIMESTAMP}" \
  --arg journal "${journal_logs}" \
  --arg services "${service_status}" \
  --arg disk "${disk_usage}" \
  --arg memory "${memory_usage}" \
  --arg uptime_info "${system_uptime}" \
  '{
    text: ("Analyze these logs and metrics from the KPBJ " + $env + " server.\nCollection time: " + $timestamp + "\nLookback window: " + $interval + " minutes\n\n--- JOURNAL LOGS (last " + $interval + " minutes) ---\n" + $journal + "\n\n--- SERVICE STATUSES ---\n" + $services + "\n\n--- DISK USAGE ---\n" + $disk + "\n\n--- MEMORY ---\n" + $memory + "\n\n--- UPTIME ---\n" + $uptime_info)
  }' | jq -r '.text')

payload=$(jq -n \
  --arg system_prompt "${SYSTEM_PROMPT}" \
  --arg user_message "${user_message}" \
  '{
    system_instruction: {
      parts: [{ text: $system_prompt }]
    },
    contents: [{
      parts: [{ text: $user_message }]
    }],
    generationConfig: {
      temperature: 0.1,
      maxOutputTokens: 4096
    }
  }')

# ── 5. Call the Gemini API ───────────────────────────────────

api_url="https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"

http_response=$(curl -s -w "\n%{http_code}" \
  --max-time 60 \
  --retry 2 \
  --retry-delay 5 \
  -H "Content-Type: application/json" \
  -H "x-goog-api-key: ${GEMINI_API_KEY}" \
  -d @<(printf '%s' "${payload}") \
  "${api_url}") || true

# Split response body and HTTP status code.
http_code=$(echo "${http_response}" | tail -n1)
response_body=$(echo "${http_response}" | sed '$d')

# ── 6. Parse Gemini response ────────────────────────────────

if [[ -z "${http_code}" || "${http_code}" -ne 200 ]]; then
  echo "[${TIMESTAMP}] Gemini API returned HTTP ${http_code}" >&2
  analysis="SUBJECT: Watchdog API error (HTTP ${http_code}) on ${WATCHDOG_ENV}
The watchdog script failed to reach the Gemini API.

HTTP status: ${http_code}
Response body (truncated):
$(echo "${response_body}" | head -c 2000)

This means log analysis did not run. Check the API key and network connectivity."
else
  analysis=$(echo "${response_body}" | jq -r '.candidates[0].content.parts[0].text // empty') || true

  if [[ -z "${analysis}" ]]; then
    echo "[${TIMESTAMP}] Failed to extract text from Gemini response" >&2
    analysis="SUBJECT: Watchdog failed to parse Gemini response on ${WATCHDOG_ENV}
The Gemini API returned HTTP 200 but the response could not be parsed.

Raw response (truncated):
$(echo "${response_body}" | head -c 2000)

This may indicate an API schema change or an unexpected error format."
  fi
fi

# ── 7. Check result and notify ───────────────────────────────

# Trim whitespace for comparison.
analysis_trimmed=$(echo "${analysis}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

if [[ "${analysis_trimmed}" == "NO_ISSUES" ]]; then
  echo "[${TIMESTAMP}] Watchdog complete: no issues detected."
  exit 0
fi

echo "[${TIMESTAMP}] Watchdog detected issues, sending alert."

# Parse the SUBJECT line from the first line of the response.
subject_line=$(echo "${analysis}" | head -n1)
if [[ "${subject_line}" == SUBJECT:* ]]; then
  raw_subject=$(echo "${subject_line#SUBJECT: }" | tr -d '\n\r')
  email_subject=$(echo "${raw_subject}" | head -c 78)
  email_body=$(echo "${analysis}" | tail -n +2)
  # If the subject was truncated, prepend the full line to the body.
  if [[ "${#raw_subject}" -gt 78 ]]; then
    email_body=$(printf '%s\n\n%s' "${raw_subject}" "${email_body}")
  fi
else
  # No SUBJECT prefix — use a generic subject and include full text as body.
  email_subject="Watchdog alert on ${WATCHDOG_ENV}"
  email_body="${analysis}"
fi

# Compose and send the email.
{
  echo "From: KPBJ Watchdog <noreply@kpbj.fm>"
  echo "To: ${WATCHDOG_RECIPIENT}"
  echo "Subject: [KPBJ ${WATCHDOG_ENV}] ${email_subject}"
  echo "Content-Type: text/plain; charset=utf-8"
  echo ""
  echo "${email_body}"
  echo ""
  echo "---"
  echo "KPBJ Watchdog | ${WATCHDOG_ENV} | ${TIMESTAMP}"
} | msmtp -C "${MSMTP_CONFIG}" "${WATCHDOG_RECIPIENT}"

echo "[${TIMESTAMP}] Alert sent to ${WATCHDOG_RECIPIENT}."
