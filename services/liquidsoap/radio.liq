#!/usr/bin/env liquidsoap

# KPBJ 95.9 FM - Liquidsoap Radio Automation
#
# The web API plans the timeline. Liquidsoap plays it and makes no timing
# decisions of its own.
#
# The timeline is a list of segments. A segment runs from one boundary to the
# next, and its items fill it with no gaps. Every segment is at least 30
# minutes long. /now returns one segment in `current`, and the whole segment
# after it in `next`.
#
#   1. At startup, get the segment that airs now. Its first item has a cue in
#      for the time that already went by, so a restart keeps the schedule.
#   2. While a segment is on the air, download the files of its next segment
#      and hold them. A file that does not download is tried again every 5
#      seconds until the check below.
#   3. 10 seconds before the segment ends, get the next segment again, and
#      compare its id with the id of the held one. If they are the same, use
#      the held files. If they are not, download the new segment.
#   4. At the boundary, replace the queue with the next segment, and stop the
#      item on air. Then go to step 2 with the new segment.
#   5. If the check in step 3 fails, play the held segment. If nothing is held,
#      then at the boundary get the segment that airs now. Retry every 5
#      seconds until the API answers.

settings.init.allow_root.set(true)

# Level 4 adds about 8 lines for each request. A file that does not download
# then writes thousands of lines an hour. They push other services out of the
# journal, and Loki keeps them for 30 days.
settings.log.level.set(3)

# A failure that can change what airs logs with log.critical, at level 1. The
# Liquidsoap alert in nixos/monitoring.nix matches levels 0 and 1. Other
# messages log with log.important, at level 3.

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

# State: last track metadata, re-sent on Icecast reconnect so metadata
# survives connection drops (e.g. Icecast container restart).
last_metadata = ref([])

# The only source. It plays the items of the current segment in order.
#
# A playlist, loaded with set_queue. set_queue adds requests that are already
# resolved, so no transition in a segment waits on storage. A queue that
# fetched each file as the previous one ended ran short in the middle of a
# break.
#
# There is no buffer and no fallback switch. In Liquidsoap 2.3.3 both replay
# old audio and old metadata after a switch. When the queue is empty, mksafe
# below plays silence.
#
# loop=false plays the list once and then goes quiet.
timeline = playlist.list(id="timeline", loop=false, [])

# Build a request from one item of a segment, with its metadata as annotations.
#
# The annotate values reach Icecast as ICY metadata and come back to us on the
# on_track callback, which posts them to /played.
#
# break_item_id is empty for an item that is not a break item. The web service
# reads an empty value as no id.
#
# Liquidsoap applies liq_cue_in and liq_cue_out when it resolves the request.
# The API gives both as offsets into the file.
def item_to_request(item)
  url = rewrite_url(item.url)
  bid = item.break_item_id ?? 0
  break_item_id = if bid == 0 then "" else string.of_int(bid) end
  cue_in = if item.cue_in > 0 then ",liq_cue_in=\"#{item.cue_in}\"" else "" end
  cue_out_at = item.cue_out ?? -1
  cue_out = if cue_out_at >= 0 then ",liq_cue_out=\"#{cue_out_at}\"" else "" end
  annotated = "annotate:title=\"#{item.title}\",artist=\"#{item.artist}\",source_type=\"#{item.type}\",source_url=\"#{url}\",break_item_id=\"#{break_item_id}\",planned_start=\"#{item.planned_start}\"#{cue_in}#{cue_out}:#{url}"
  request.create(annotated)
end

# Get one segment from the API.
#
# An empty `from` asks for the segment that airs now. Otherwise `from` is the
# segment_end of the current segment, exactly as the API sent it, and the API
# returns the segment that starts there.
#
# Returns null on any status other than 200, or on a body that does not parse.
# end_at is the wall clock time when the segment ends. It comes from
# ends_in_seconds and the time of the answer. The answer comes a few
# milliseconds after the API reads its clock, so end_at is a little late and
# never early. An early end_at made the boundary ask /now for the current
# segment while the old segment still had a few milliseconds left.
#
# next is the segment after it, or null when the API has none.
def fetch_segment(from)
  query = if from == "" then "" else "?from=#{url.encode(from)}" end
  response = http.get(timeout=5., "#{api_base}/now#{query}")
  answered_at = time()
  if response.status_code != 200 then
    log.critical("[KPBJ] /now#{query} returned status #{response.status_code}")
    null()
  else
    try
      # Liquidsoap has no type aliases, so the segment type is written twice.
      let json.parse (answer : {
        current: {
          id: string,
          segment_end: string,
          ends_in_seconds: float,
          items: [{
            type: string,
            url: string,
            title: string,
            artist: string,
            cue_in: int,
            cue_out: int?,
            break_item_id: int?,
            planned_start: string
          }]
        },
        next: {
          id: string,
          segment_end: string,
          ends_in_seconds: float,
          items: [{
            type: string,
            url: string,
            title: string,
            artist: string,
            cue_in: int,
            cue_out: int?,
            break_item_id: int?,
            planned_start: string
          }]
        }?
      }) = response
      def timed(s)
        {id = s.id, segment_end = s.segment_end, end_at = answered_at + s.ends_in_seconds, items = s.items}
      end
      current = answer.current
      log.important("[KPBJ] /now#{query}: segment #{current.id}, #{list.length(current.items)} items, ends in #{current.ends_in_seconds}s")
      null(timed(current).{next = null.map(timed, answer.next)})
    catch _ do
      log.critical("[KPBJ] Failed to parse /now#{query} response")
      null()
    end
  end
end

# Download every file of a segment. Returns the requests that resolved.
#
# This blocks until all files are local. Each pass tries the items that did
# not resolve yet, so one bad item does not stop the items after it. A URL
# that fails in a pass is not tried again for the other items in that pass.
# Repeated filler has one URL for many items. A new pass starts every 5
# seconds until the wall clock time `until`. A time in the past gives each
# item one try. After the last pass, an item that did not resolve is dropped.
# The items after it then play early, and the stream goes quiet before the
# boundary.
def resolve_items(items, until)
  def resolve_one(item)
    r = item_to_request(item)
    if request.resolve(content_type=timeline, r) then
      null(r)
    else
      request.destroy(r)
      log.important("[KPBJ] Could not resolve #{item.url}")
      null()
    end
  end
  def rec pass(pairs)
    failed_urls = ref([])
    def try_missing(pair)
      let (item, r) = pair
      if null.defined(r) or list.mem(item.url, failed_urls()) then
        pair
      else
        fresh = resolve_one(item)
        if not null.defined(fresh) then
          failed_urls := item.url :: failed_urls()
        end
        (item, fresh)
      end
    end
    pairs = list.map(try_missing, pairs)
    missing = list.exists(fun(p) -> not null.defined(snd(p)), pairs)
    if missing and time() + 5. < until then
      log.important("[KPBJ] Some items did not resolve, retry in 5 seconds")
      thread.pause(5.)
      pass(pairs)
    else
      def log_dropped(p)
        if not null.defined(snd(p)) then
          log.critical("[KPBJ] Dropped #{fst(p).url}, it did not download")
        end
      end
      list.iter(log_dropped, pairs)
      pairs
    end
  end
  list.filter_map(snd, pass(list.map(fun(item) -> (item, null()), items)))
end

# The held segment: the downloaded requests of the next segment, and its id.
# The API changes the id of a segment only when what airs in it changes.
# held_id is empty when nothing is held. A hold can miss an item that did not
# download. The swap then plays the segment without that item.
held_id = ref("")
held_requests = ref([])

# Free the held requests and their files.
def drop_held()
  list.iter(request.destroy, held_requests())
  held_requests := []
  held_id := ""
end

# Download the files of a segment and hold them. A null segment holds nothing.
def hold(next, until)
  drop_held()
  if null.defined(next) then
    n = null.get(next)
    requests = resolve_items(n.items, until)
    held_requests := requests
    held_id := n.id
    log.important("[KPBJ] Holding #{list.length(requests)} of #{list.length(n.items)} items of segment #{n.id}")
  end
end

# Take the held requests and hold nothing.
def take_held()
  requests = held_requests()
  held_requests := []
  held_id := ""
  requests
end

# The requests to play for a segment. Use the held ones when the held segment
# has the same id. Otherwise download the segment now.
def requests_for(segment)
  if held_id() != "" and segment.id == held_id() then
    take_held()
  else
    log.important("[KPBJ] Segment #{segment.id} is not the held segment #{held_id()}, download it now")
    drop_held()
    resolve_items(segment.items, 0.)
  end
end

# Replace the queue with resolved requests, and stop the item on air.
#
# The skip goes only to an item of the old segment. A skip on an idle source
# does not go away. It ends the next track that the source gets, as soon as
# that track is ready. Liquidsoap 2.3.3 acts this way. Thus there is no skip
# when nothing is on air, or when the first new item already started.
def swap(requests)
  old = timeline.queue()
  timeline.set_queue(requests)
  list.iter(request.destroy, old)
  new_ids = list.map(request.id, requests)
  on_air = timeline.current()
  if null.defined(on_air) and not list.mem(request.id(null.get(on_air)), new_ids) then
    timeline.skip()
  end
  log.important("[KPBJ] Swapped in #{list.length(requests)} items")
end

# The function that starts the schedule again. It is set below, after plan.
start_schedule = ref(fun () -> ())

# Start the schedule again 5 seconds after an error in one of its tasks.
#
# The schedule is one chain of tasks, and each task starts the next. An
# uncaught error ends its task and leaves the process running. Without this
# handler, the chain then stops, and mksafe plays silence until a manual
# restart. Every task of the chain uses this handler.
def rec on_schedule_error(err)
  log.critical("[KPBJ] Error in the schedule: #{err.kind}: #{err.message}. Start again in 5 seconds")
  thread.run(fast=false, delay=5., on_error=fun(e) -> on_schedule_error(e), fun() -> begin
    start = start_schedule()
    start()
  end)
end

# Run f at a wall clock time.
#
# thread.run(delay=...) counts on the monotonic clock. A boundary is a wall
# clock time, so this function checks the wall clock again at least once each
# second. It sleeps for the exact time left when that is less than a second.
# A time in the past runs f at once.
def run_at(target, f)
  def check()
    left = target - time()
    if left <= 0. then
      f()
      -1.
    else
      min(left, 1.)
    end
  end
  def on_error(err)
    on_schedule_error(err)
    -1.
  end
  thread.run.recurrent(fast=false, delay=0., on_error=on_error, check)
end

# Get the segment that airs now and put it on the air. Retry every 5 seconds
# until the API answers. Then call plan with the segment.
#
# Used at startup, after an error in the schedule, and at a boundary when the
# check failed and nothing is held.
def rec play_current(plan)
  drop_held()
  segment = fetch_segment("")
  if null.defined(segment) then
    s = null.get(segment)
    swap(resolve_items(s.items, 0.))
    plan({segment_end = s.segment_end, end_at = s.end_at, next = s.next})
  else
    log.important("[KPBJ] No current segment, retry in 5 seconds")
    thread.run(fast=false, delay=5., on_error=on_schedule_error, fun() -> play_current(plan))
  end
end

# Hold the next segment and plan the swap, for the segment that just went on
# the air.
#
# The check runs 10 seconds before the segment ends. When it is done, the swap
# waits for the boundary. A check or a download that is slower than 10 seconds
# swaps when it is done. A hold that is slower than the segment runs the check
# when the hold is done.
def rec plan(current)
  check_at = current.end_at - 10.
  hold(current.next, check_at)
  def check()
    fresh = fetch_segment(current.segment_end)
    if null.defined(fresh) then
      f = null.get(fresh)
      requests = requests_for(f)
      run_at(current.end_at, fun() -> begin
        swap(requests)
        plan({segment_end = f.segment_end, end_at = f.end_at, next = f.next})
      end)
    elsif held_id() != "" then
      log.critical("[KPBJ] Check failed, play the held segment")
      n = null.get(current.next)
      requests = take_held()
      run_at(current.end_at, fun() -> begin
        swap(requests)
        plan({segment_end = n.segment_end, end_at = n.end_at, next = null()})
      end)
    else
      log.critical("[KPBJ] Check failed and nothing is held, get the current segment at the boundary")
      run_at(current.end_at, fun() -> play_current(plan))
    end
  end
  run_at(check_at, check)
end

start_schedule := fun () -> play_current(plan)

thread.run(fast=false, on_error=on_schedule_error, fun() -> play_current(plan))

# Log track changes and POST to web service.
#
# Filler is not posted. It is not content that the playback history or the
# delivery counts track.
#
# The POST runs in a separate task. on_track runs in the streaming loop, and a
# slow POST there stops the audio for up to the 5 second timeout. The values
# for the body are read before the task starts.
timeline.on_track(fun(m) -> begin
  title = m["title"] ?? "unknown"
  artist = m["artist"] ?? ""
  source_type = m["source_type"] ?? "unknown"
  source_url = m["source_url"] ?? (m["filename"] ?? "")
  break_item_id = m["break_item_id"]
  planned_start = m["planned_start"]

  last_metadata := m

  log.important("[KPBJ] Now playing: #{artist} - #{title} (#{source_type})")

  # POST to web service if playout_secret is configured
  if playout_secret != "" and source_type != "filler" then
    t = time.utc()
    p = fun(n) -> string.of_int(digits=2, n)
    started_at = "#{t.year}-#{p(t.month)}-#{p(t.day)}T#{p(t.hour)}:#{p(t.min)}:#{p(t.sec)}Z"

    body = json.stringify({
      prTitle = title,
      prArtist = artist,
      prSourceType = source_type,
      prSourceUrl = source_url,
      prStartedAt = started_at,
      prBreakItemId = break_item_id,
      prPlannedStart = planned_start
    })

    def post_played()
      response = http.post(
        timeout=5.,
        headers=[
          ("Content-Type", "application/json"),
          ("X-Playout-Secret", playout_secret)
        ],
        data=body,
        "#{api_base}/played"
      )
      if response.status_code < 200 or response.status_code >= 300 then
        log.critical("[KPBJ] Playout POST failed: #{response.status_code}")
      end
    end

    def on_post_error(err)
      log.critical("[KPBJ] Playout POST error: #{err.kind}: #{err.message}")
    end

    thread.run(fast=false, on_error=on_post_error, post_played)
  end
end)

# --- Audio Processing Chain ---

# Normalize volume levels across tracks (target -14 LUFS-ish)
radio = normalize(target=0., window=0.5, gain_min=-6., gain_max=6., timeline)

# Compress dynamic range for consistent broadcast loudness
radio = compress(threshold=-10., ratio=3., attack=10., release=200., gain=3., radio)

# Hard limit to prevent clipping
radio = limit(threshold=-1., attack=5., release=50., radio)

# Play silence when the timeline has nothing on the air
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
