#!/usr/bin/env liquidsoap

# KPBJ 95.9 FM - Liquidsoap Radio Automation
#
# Polls the web API at :00 and :30 for scheduled shows.
# Uses request.queue() with manual push to avoid prefetch bugs.
# Tracks last_scheduled to prevent replaying the same show.

settings.init.allow_root.set(true)

settings.server.telnet.set(true)
settings.log.level.set(4)

# Configuration from environment
api_base = environment.get("API_BASE") ?? "http://localhost:4000/api/playout"
icecast_host = environment.get("ICECAST_HOST") ?? "127.0.0.1"
icecast_port = int_of_string(environment.get("ICECAST_PORT") ?? "8000")
icecast_password = environment.get("ICECAST_PASSWORD") ?? "hackme"
icecast_mount = environment.get("ICECAST_MOUNT") ?? "/stream"
playout_secret = environment.get("PLAYOUT_SECRET") ?? ""

# Rewrite localhost URLs to be reachable from the VM/container.
# REWRITE_HOST is set by the NixOS streaming module (e.g. "10.0.2.2" for QEMU).
# When unset, localhost URLs pass through unchanged (native host usage).
rewrite_host = environment.get("REWRITE_HOST") ?? ""
def rewrite_url(url)
  if rewrite_host == "" then url
  else string.replace(pattern="localhost", fun(_) -> rewrite_host, url)
  end
end

# State: URL of the last scheduled show we played (to avoid duplicates)
last_scheduled = ref("")

# Consecutive "no show" poll count. We only clear last_scheduled after 2
# consecutive null responses, so a single transient API/DB error mid-show
# won't cause the episode to restart from the beginning.
null_poll_count = ref(0)

# State: last track metadata, re-sent on Icecast reconnect so metadata
# survives connection drops (e.g. Icecast container restart).
last_metadata = ref([])

# Fetch a fallback track from the API
def get_fallback()
  response = http.get(timeout=5., "#{api_base}/fallback")
  if response.status_code == 200 then
    try
      let json.parse (data : {url: string, title: string?, artist: string?}) = response
      title = data.title ?? "KPBJ 95.9 FM"
      artist = data.artist ?? "Community Radio"
      url = rewrite_url(data.url)
      annotated = "annotate:title=\"#{title}\",artist=\"#{artist}\",source_type=\"ephemeral\":#{url}"
      request.create(annotated)
    catch _ do
      log.info("[KPBJ] Failed to parse fallback response")
      null()
    end
  else
    log.info("[KPBJ] Fallback API returned status #{response.status_code}")
    null()
  end
end

# Fallback source - always has something to play
# prefetch=1 for accurate metadata timing (higher values queue tracks ahead)
fallback_source = request.dynamic(prefetch=1, get_fallback)

# Add buffer to absorb latency from HTTP downloads (5s buffer, 10s max)
fallback_source = buffer(buffer=5., max=10., fallback_source)

# Scheduled source - queue that we manually push to
scheduled_queue = request.queue(id="scheduled")

# Buffered version for playback (keeps queue reference for .push)
scheduled_source = buffer(buffer=5., max=10., scheduled_queue)

# Poll the API for the current scheduled show
def poll()
  log.important("[KPBJ] Polling schedule...")
  response = http.get(timeout=5., "#{api_base}/now")

  if response.status_code != 200 then
    log.important("[KPBJ] Schedule API returned status #{response.status_code}")
  else
    body = string.trim(response)

    # API returns "null" when nothing is scheduled
    if body == "null" or body == "" then
      log.info("[KPBJ] No show currently scheduled")
      null_poll_count := null_poll_count() + 1
      if null_poll_count() >= 2 and last_scheduled() != "" then
        log.important("[KPBJ] Clearing last_scheduled after #{null_poll_count()} consecutive empty polls")
        last_scheduled := ""
      end
    else
      try
        let json.parse (data : {url: string, title: string?, artist: string?}) = body
        url = rewrite_url(data.url)

        null_poll_count := 0
        if url == last_scheduled() then
          log.info("[KPBJ] Same show as before, not replaying: #{url}")
        else
          log.important("[KPBJ] New show scheduled: #{url}")
          last_scheduled := url
          title = data.title ?? "KPBJ 95.9 FM"
          artist = data.artist ?? ""
          annotated = "annotate:title=\"#{title}\",artist=\"#{artist}\",source_type=\"episode\":#{url}"
          scheduled_queue.push(request.create(annotated))
        end
      catch _ do
        log.info("[KPBJ] Failed to parse schedule response: #{body}")
      end
    end
  end
end

# Poll at :00 and :30 each hour
thread.when({0m0s}, poll)
thread.when({30m0s}, poll)

# Poll at startup after a short delay to let fallback stabilize
thread.run(delay=3., poll)

# Main output - scheduled takes priority, fallback fills gaps
# track_sensitive=false means switch immediately when scheduled content arrives
radio = fallback(track_sensitive=false, [scheduled_source, fallback_source])

# Log track changes and POST to web service
# NOTE: Before crossfade so metadata is present. ~1s timing offset from crossfade is acceptable.
radio = source.on_track(radio, fun(m) -> begin
  title = m["title"] ?? "unknown"
  artist = m["artist"] ?? ""
  source_type = m["source_type"] ?? "unknown"
  source_url = m["filename"] ?? ""

  last_metadata := m

  log.important("[KPBJ] Now playing: #{artist} - #{title} (#{source_type})")

  # POST to web service if playout_secret is configured
  if playout_secret != "" then
    started_at = time.string("%Y-%m-%dT%H:%M:%SZ")

    body = json.stringify({
      prTitle = title,
      prArtist = artist,
      prSourceType = source_type,
      prSourceUrl = source_url,
      prStartedAt = started_at
    })

    ignore(http.post(
      timeout=5.,
      headers=[
        ("Content-Type", "application/json"),
        ("X-Playout-Secret", playout_secret)
      ],
      data=body,
      "#{api_base}/played"
    ))
  end
end)

# Add crossfade between tracks (1s fade) to smooth transitions
radio = crossfade(duration=1., radio)

# --- Audio Processing Chain ---

# Normalize volume levels across tracks (target -14 LUFS-ish)
radio = normalize(target=0., window=0.5, gain_min=-6., gain_max=6., radio)

# Compress dynamic range for consistent broadcast loudness
radio = compress(threshold=-10., ratio=3., attack=10., release=200., gain=3., radio)

# Hard limit to prevent clipping
radio = limit(threshold=-1., attack=5., release=50., radio)

# Safety fallback in case both sources fail
radio = mksafe(radio)

# Wrap with insert_metadata so we can re-send metadata on Icecast reconnect
radio = insert_metadata(radio)

# Output to Icecast
output.icecast(
  %mp3(bitrate=128),
  host=icecast_host,
  port=icecast_port,
  password=icecast_password,
  mount=icecast_mount,
  name="KPBJ 95.9 FM",
  description="Community Radio for Sun Valley and Burbank",
  url="https://kpbj.fm",
  genre="Eclectic",
  on_connect=fun() -> begin
    m = last_metadata()
    if list.length(m) > 0 then
      log.important("[KPBJ] Reconnected to Icecast, re-sending metadata")
      thread.run(delay=2., fun() -> begin
        radio.insert_metadata(m)
      end)
    end
  end,
  radio
)
