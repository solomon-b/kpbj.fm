#!/usr/bin/env python3
"""Integration tests for services/liquidsoap/radio.liq.

The harness runs the real script with the pinned Liquidsoap binary. It starts a
stub playout API and a local Icecast. libfaketime sets the wall clock of
Liquidsoap to a few seconds before a segment boundary, so the prefetch and the
swap at the boundary occur during the test. The stub records each /played
POST, and each scenario checks that record.

The clock runs at normal speed, and the harness jumps it forward over idle
time. A faster clock does not work, because the audio plays at normal speed.

Two rules apply to a jump:

  - Liquidsoap timers inside the skipped time fire at once. Jump to a time
    before the boundary under test.
  - The audio does not jump. An item on air keeps its position, so it runs
    late after a jump. Jump only when the swap at the next boundary stops the
    item on air.

The audio clock uses the monotonic clock, which libfaketime leaves alone, so
the audio plays at normal speed across a jump.

Run it with `just test-liquidsoap`. The recipe supplies these variables:

  LIQUIDSOAP_BIN, FAKETIME_LIB, ICECAST_BIN, FFMPEG_BIN

Scenarios run one at a time, because they use the same stub port and the same
Icecast mount.

A check can carry a known issue. Such a check describes the correct behavior,
and it fails today. Its failure is reported as KNOWN and does not fail the run.
When it passes, the run fails, so that the marker gets removed.

Usage: run.py LIQ_FILE [SCENARIO ...]
"""

import datetime as dt
import hashlib
import http.server
import json
import os
import pathlib
import shutil
import signal
import socketserver
import subprocess
import sys
import tempfile
import threading
import time
import urllib.parse

STUB_PORT = 18080
ICECAST_PORT = 18000

LIQUIDSOAP = os.environ["LIQUIDSOAP_BIN"]
FAKETIME_LIB = os.environ["FAKETIME_LIB"]
ICECAST = os.environ["ICECAST_BIN"]
FFMPEG = os.environ["FFMPEG_BIN"]

DAY = dt.date(2026, 10, 21)

# Item types that the real API gives a break item id.
BREAK_ITEM_TYPES = {"psa", "underwriting", "announcement", "show_promo"}


def at(hms):
    """Return a fake local time on the test day."""
    h, m, s = (int(x) for x in hms.split(":"))
    return dt.datetime.combine(DAY, dt.time(h, m, s))


def iso(t):
    """Write a fake local time as a UTC ISO 8601 string, as the real API does."""
    return t.astimezone(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


# --- Checks and steps --------------------------------------------------------


def airs(title, kind, lo, hi, known_issue=None):
    """The item must appear in /played between lo and hi."""
    return dict(check="airs", want=True, title=title, kind=kind, lo=at(lo), hi=at(hi),
                known_issue=known_issue)


def absent(title, kind, lo, hi, known_issue=None):
    """The item must not appear in /played between lo and hi."""
    return dict(check="airs", want=False, title=title, kind=kind, lo=at(lo), hi=at(hi),
                known_issue=known_issue)


def on_time(title, kind, hms, late=2, early=0.5, known_issue=None):
    """The item must post at most `late` seconds after its planned start hms.

    By default it may post up to half a second early. Liquidsoap reports a
    track start up to a few audio frames before the audio plays, and the items
    of a segment can get a few frames ahead of the wall clock. Set early=0
    when the item must not post before its planned start.
    """
    t = at(hms)
    return dict(check="airs", want=True, title=title, kind=kind,
                lo=t - dt.timedelta(seconds=early), hi=t + dt.timedelta(seconds=late),
                known_issue=known_issue)


def posted_planned_start(title, hms, known_issue=None):
    """Each /played POST of the title must carry this planned start.

    hms is a fake local time. The check compares it as the UTC ISO 8601 string
    that the stub sends. The title must post at least once.
    """
    return dict(check="planned", title=title, planned=iso(at(hms)), hms=hms,
                known_issue=known_issue)


def jump(when, to):
    """At fake time `when`, move the clock forward to `to`."""
    return (at(when), "jump", at(to))


def restart(when):
    """At fake time `when`, stop Liquidsoap and start it again."""
    return (at(when), "restart", None)


# --- Scenarios ---------------------------------------------------------------
#
# start:      fake time when Liquidsoap starts. Startup takes about 5 seconds.
# end:        fake time when the run stops.
# segments:   (start, end, [(title, kind, secs, cue_out)]). secs is the audio
#             length. cue_out is None, or the second where the item stops. An
#             item airs for cue_out seconds, or for secs when cue_out is None.
#             The items of a segment air one after another from its start.
#             /now with no `from` returns the segment that holds the fake time,
#             resumed with a cue_in on its first item. /now with `from` returns
#             the segment that starts there. Each answer also has the whole
#             segment after it in `next`. Segments are often shorter than a
#             real slot, so that a run is short.
# changes:    (when, segment) pairs. From fake time `when`, the stub answers
#             with this segment in place of the segment with the same start.
# fail_from:  segment starts where /now?from= answers 503.
# slow_audio: {title: seconds} the stub waits before it sends the audio of an
#             item, as slow storage does.
# fail_audio: {title: count} the first `count` downloads of the audio of an
#             item get a 500.
# steps:      jump(...) and restart(...) steps.
# checks:     airs(...), absent(...), on_time(...), and posted_planned_start(...)
#             checks on the /played record.

SHOW_B = (at("17:00:00"), at("17:30:00"), [("Show B", "episode", 120, None)])

# A 30 minute slot from 16:30. The episode is 27 minutes long. Filler covers
# the minute before the break. The break is a station ID, a PSA, and filler.
SLOT_A = (at("16:30:00"), at("17:00:00"), [
    ("Show A", "episode", 1620, None),
    ("Filler", "filler", 180, 60),
    ("Station ID", "station_id", 10, None),
    ("PSA One", "psa", 30, None),
    ("Filler", "filler", 180, 80),
])

# A slot from 16:30 that the episode fills to the end.
FULL_SLOT_A = (at("16:30:00"), at("17:00:00"), [("Show A", "episode", 1800, None)])

SCENARIOS = {
    # Liquidsoap starts in a short segment and swaps to the next one at 16:58.
    # That segment is an episode, filler cut short, a station ID, a PSA, and
    # filler. Each item must post near its planned start, and the next segment
    # must start at 17:00. Filler never posts.
    "plays-segment-in-order": dict(
        start=at("16:57:40"),
        end=at("17:00:10"),
        segments=[
            (at("16:57:00"), at("16:58:00"), [("Show Z", "episode", 60, None)]),
            (at("16:58:00"), at("17:00:00"), [
                ("Show A", "episode", 30, None),
                ("Filler", "filler", 180, 20),
                ("Station ID", "station_id", 10, None),
                ("PSA One", "psa", 30, None),
                ("Filler", "filler", 180, 30),
            ]),
            SHOW_B,
        ],
        steps=[],
        checks=[
            on_time("Show A", "episode", "16:58:00"),
            on_time("Station ID", "station_id", "16:58:50"),
            on_time("PSA One", "psa", "16:59:00"),
            on_time("Show B", "episode", "17:00:00"),
            absent("Filler", "filler", "16:57:40", "17:00:10"),
        ],
    ),
    # The next episode must start at the boundary.
    "next-segment-at-boundary": dict(
        start=at("16:59:30"),
        end=at("17:00:10"),
        segments=[FULL_SLOT_A, SHOW_B],
        steps=[],
        checks=[on_time("Show B", "episode", "17:00:00")],
    ),
    # Liquidsoap restarts 20 minutes into Show A. The episode must post at the
    # restart, and the next segment must start on time. After the jump, Show A
    # is still on air, so the swap must stop it.
    "restart-mid-episode": dict(
        start=at("16:50:00"),
        end=at("17:00:10"),
        segments=[SLOT_A, SHOW_B],
        steps=[jump("16:50:30", "16:59:30")],
        checks=[
            airs("Show A", "episode", "16:50:00", "16:50:10"),
            on_time("Show B", "episode", "17:00:00"),
        ],
    ),
    # Liquidsoap restarts during PSA One, with 10 seconds of it left. The PSA
    # must post again with the same planned start. PSA Two must then start on
    # time, which it does only if PSA One resumed with a cue in. From 0:00,
    # PSA One would play for 30 seconds, and PSA Two would start 20 seconds
    # late. The next segment must start on time.
    "restart-mid-break": dict(
        start=at("16:57:50"),
        end=at("17:00:10"),
        segments=[
            (at("16:30:00"), at("17:00:00"), [
                ("Show A", "episode", 1620, None),
                ("Filler", "filler", 180, 60),
                ("Station ID", "station_id", 10, None),
                ("PSA One", "psa", 30, None),
                ("PSA Two", "psa", 30, None),
                ("Filler", "filler", 180, 50),
            ]),
            SHOW_B,
        ],
        steps=[restart("16:58:30")],
        checks=[
            airs("PSA One", "psa", "16:58:10", "16:58:15"),
            airs("PSA One", "psa", "16:58:30", "16:58:35"),
            posted_planned_start("PSA One", "16:58:10"),
            on_time("PSA Two", "psa", "16:58:40", early=0),
            on_time("Show B", "episode", "17:00:00"),
        ],
    ),
    # The audio of the next episode takes 8 seconds to download. Liquidsoap
    # holds it before the check, so it must start on time, and Show A must not
    # post after the boundary.
    "slow-next-download": dict(
        start=at("16:59:30"),
        end=at("17:00:15"),
        segments=[FULL_SLOT_A, SHOW_B],
        slow_audio={"Show B": 8},
        steps=[],
        checks=[
            on_time("Show B", "episode", "17:00:00"),
            absent("Show A", "episode", "17:00:00", "17:00:15"),
        ],
    ),
    # The check of the next segment gets a 503. Liquidsoap must play the held
    # segment, so the next episode starts on time.
    "check-fails": dict(
        start=at("16:59:30"),
        end=at("17:00:20"),
        segments=[FULL_SLOT_A, SHOW_B],
        fail_from=[at("17:00:00")],
        steps=[],
        checks=[
            on_time("Show B", "episode", "17:00:00"),
            absent("Show A", "episode", "17:00:00", "17:00:20"),
        ],
    ),
    # The next segment changes after Liquidsoap holds it. The check must see
    # the change, so the new episode airs at the boundary and the held one
    # does not air.
    "next-segment-changes": dict(
        start=at("16:59:30"),
        end=at("17:00:15"),
        segments=[FULL_SLOT_A, SHOW_B],
        changes=[(at("16:59:45"), (at("17:00:00"), at("17:30:00"), [("Show B New", "episode", 120, None)]))],
        steps=[],
        checks=[
            on_time("Show B New", "episode", "17:00:00", late=3),
            absent("Show B", "episode", "16:59:30", "17:00:15"),
        ],
    ),
    # The first 3 downloads of the next episode fail. The hold must try again
    # until one works, so the episode starts on time. With one try at the hold
    # and one at the check, it does not air.
    "next-download-retries": dict(
        start=at("16:59:15"),
        end=at("17:00:15"),
        segments=[FULL_SLOT_A, SHOW_B],
        fail_audio={"Show B": 3},
        steps=[],
        checks=[on_time("Show B", "episode", "17:00:00")],
    ),
    # The middle item of the next segment never downloads, and the items on
    # each side of it take 8 seconds each. The hold must download both while
    # it tries the bad item again, and the check must use the partial hold.
    # Show B then starts at the boundary, and Ephemeral C follows it. If the
    # hold waits on the bad item or the check downloads the segment again, the
    # swap is late.
    "next-item-never-downloads": dict(
        start=at("16:59:15"),
        end=at("17:00:30"),
        segments=[
            FULL_SLOT_A,
            (at("17:00:00"), at("17:30:00"), [
                ("Show B", "episode", 20, None),
                ("Bad Item", "ephemeral", 10, None),
                ("Ephemeral C", "ephemeral", 120, None),
            ]),
        ],
        slow_audio={"Show B": 8, "Ephemeral C": 8},
        fail_audio={"Bad Item": 1000},
        steps=[],
        checks=[
            on_time("Show B", "episode", "17:00:00"),
            on_time("Ephemeral C", "ephemeral", "17:00:20"),
            absent("Bad Item", "ephemeral", "16:59:15", "17:00:30"),
            absent("Show A", "episode", "17:00:00", "17:00:30"),
        ],
    ),
    # An hour with no show. Ephemeral uploads fill it, an automation break
    # follows, and more ephemeral uploads start at 17:00. Each item must post
    # near its planned start.
    "unscheduled-hour": dict(
        start=at("16:57:10"),
        end=at("17:00:10"),
        segments=[
            (at("16:56:30"), at("16:57:30"), [("Ephemeral Zero", "ephemeral", 60, None)]),
            (at("16:57:30"), at("17:00:00"), [
                ("Ephemeral One", "ephemeral", 20, None),
                ("Ephemeral Two", "ephemeral", 30, 10),
                ("Station ID", "station_id", 10, None),
                ("PSA One", "psa", 30, None),
                ("Underwriting One", "underwriting", 30, None),
                ("Filler", "filler", 180, 50),
            ]),
            (at("17:00:00"), at("17:30:00"), [("Ephemeral Three", "ephemeral", 60, None)]),
        ],
        steps=[],
        checks=[
            on_time("Ephemeral One", "ephemeral", "16:57:30"),
            on_time("Ephemeral Two", "ephemeral", "16:57:50"),
            on_time("Station ID", "station_id", "16:58:00"),
            on_time("PSA One", "psa", "16:58:10"),
            on_time("Underwriting One", "underwriting", "16:58:40"),
            on_time("Ephemeral Three", "ephemeral", "17:00:00"),
            absent("Filler", "filler", "16:57:10", "17:00:10"),
        ],
    ),
}


# --- Fake clock --------------------------------------------------------------


class Clock:
    """A fake wall clock that the stub and Liquidsoap share.

    libfaketime reads the offset from a file on every time call, so a write to
    that file moves the clock of Liquidsoap at once.
    """

    def __init__(self, path):
        self.path = path
        self.offset = 0

    def set(self, fake):
        self.offset = int(round(fake.timestamp() - time.time()))
        tmp = self.path.with_suffix(".tmp")
        tmp.write_text(f"{self.offset:+d}\n")
        tmp.replace(self.path)

    def now(self):
        return dt.datetime.fromtimestamp(time.time() + self.offset)

    def env(self):
        return dict(
            LD_PRELOAD=FAKETIME_LIB,
            FAKETIME_TIMESTAMP_FILE=str(self.path),
            FAKETIME_NO_CACHE="1",
            FAKETIME_DONT_FAKE_MONOTONIC="1",
        )


# --- Audio -------------------------------------------------------------------


class Audio:
    def __init__(self, root):
        self.root = root
        root.mkdir(parents=True, exist_ok=True)

    def tone(self, name, seconds, freq):
        """Make a sine tone file and return its URL on the stub."""
        path = self.root / f"{name}.mp3"
        if not path.exists():
            subprocess.run(
                [FFMPEG, "-loglevel", "error", "-y", "-f", "lavfi",
                 "-i", f"sine=frequency={freq}:duration={seconds}",
                 "-ac", "2", "-ar", "44100", "-b:a", "64k", str(path)],
                check=True,
            )
        return f"http://localhost:{STUB_PORT}/audio/{path.name}"


def slug(title, seconds):
    return f"{title.lower().replace(' ', '-')}-{seconds}s"


# --- Stub API ----------------------------------------------------------------


class Stub:
    def __init__(self, sc, audio, clock):
        self.sc = sc
        self.clock = clock
        self.played = []
        self.requests = []
        self.lock = threading.Lock()
        self.fail_from = {iso(t) for t in sc.get("fail_from", [])}
        files = {}
        next_id = 1

        def build(start, end, entries):
            nonlocal next_id
            items = []
            t = start
            for title, kind, secs, cue_out in entries:
                key = slug(title, secs)
                if key not in files:
                    files[key] = (title, audio.tone(key, secs, 220 + 110 * len(files)))
                airs_for = cue_out if cue_out is not None else secs
                break_item_id = None
                if kind in BREAK_ITEM_TYPES:
                    break_item_id = next_id
                    next_id += 1
                items.append(dict(
                    type=kind, url=files[key][1], title=title, artist="KPBJ",
                    duration_seconds=airs_for, cue_in=0, cue_out=cue_out,
                    break_item_id=break_item_id, start=t,
                ))
                t += dt.timedelta(seconds=airs_for)
            return (start, end, items)

        self.base = [build(*s) for s in sc["segments"]]
        self.changes = [(when, build(*s)) for when, s in sc.get("changes", [])]
        slow = sc.get("slow_audio", {})
        self.slow_audio = {
            url.rsplit("/", 1)[-1]: slow[title]
            for title, url in files.values() if title in slow
        }
        fail = sc.get("fail_audio", {})
        self.fail_audio = {
            url.rsplit("/", 1)[-1]: fail[title]
            for title, url in files.values() if title in fail
        }

    def segments(self, real):
        """The segments at fake time `real`, with the changes up to then."""
        out = list(self.base)
        for when, changed in self.changes:
            if real >= when:
                out = [changed if s[0] == changed[0] else s for s in out]
        return out

    def audio_fails(self, name):
        """True when this download of the file must get a 500."""
        with self.lock:
            left = self.fail_audio.get(name, 0)
            if left > 0:
                self.fail_audio[name] = left - 1
                return True
            return False

    def segment_json(self, start, end, items, now, resume):
        """Build a /now answer. A resume drops the items that ended and cues in the first."""
        out = []
        whole = now.replace(microsecond=0)
        for item in items:
            item_end = item["start"] + dt.timedelta(seconds=item["duration_seconds"])
            if resume and item_end <= whole:
                continue
            j = {k: v for k, v in item.items() if k != "start"}
            # The planned start stays the start of the whole airing, also on a
            # resume. /played uses it to count a resumed break item once.
            j["planned_start"] = iso(item["start"])
            if resume and not out:
                elapsed = int((whole - item["start"]).total_seconds())
                j["cue_in"] += elapsed
                j["duration_seconds"] -= elapsed
            out.append(j)
        # Like the API, the id comes from the whole segment, so a resume
        # keeps it.
        airing = [
            [i["url"], i["cue_in"], i["cue_out"], i["break_item_id"], iso(i["start"])]
            for i in items
        ]
        segment_id = hashlib.sha256(json.dumps([iso(start), iso(end), airing]).encode()).hexdigest()
        return {
            "id": segment_id,
            "segment_start": iso(start),
            "segment_end": iso(end),
            "ends_in_seconds": (end - now).total_seconds(),
            "items": out,
        }

    def now(self, query):
        """Answer /now. Return (status, body)."""
        real = self.clock.now()
        segments = self.segments(real)
        frm = query.get("from", [None])[0]
        if frm is None:
            found = [s for s in segments if s[0] <= real < s[1]]
            resume = True
        elif frm in self.fail_from:
            return 503, {"error": "fail_from"}
        else:
            found = [s for s in segments if iso(s[0]) == frm]
            resume = False
        if not found:
            return 503, {"error": "No segment for the request"}
        start, end, items = found[0]
        after = [s for s in segments if s[0] == end]
        return 200, {
            "current": self.segment_json(start, end, items, real, resume=resume),
            "next": self.segment_json(*after[0], real, resume=False) if after else None,
        }

    def record(self, body):
        with self.lock:
            self.played.append((self.clock.now(), body.get("prTitle"), body.get("prSourceType"),
                                body.get("prPlannedStart")))


def serve(stub, audio):
    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *a):
            pass

        def reply(self, code, payload, ctype="application/json"):
            data = payload if isinstance(payload, bytes) else json.dumps(payload).encode()
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self):
            url = urllib.parse.urlsplit(self.path)
            p = url.path
            if p.startswith("/audio/"):
                name = p.rsplit("/", 1)[-1]
                if stub.audio_fails(name):
                    return self.reply(500, {"error": "fail_audio"})
                time.sleep(stub.slow_audio.get(name, 0))
                return self.reply(200, (audio.root / name).read_bytes(), "audio/mpeg")
            if p.endswith("/now"):
                code, body = stub.now(urllib.parse.parse_qs(url.query))
                with stub.lock:
                    stub.requests.append((stub.clock.now(), self.path, code))
                return self.reply(code, body)
            self.reply(404, {"error": p})

        def do_POST(self):
            n = int(self.headers.get("Content-Length", 0))
            body = json.loads(self.rfile.read(n) or b"{}")
            if self.path.endswith("/played"):
                stub.record(body)
                self.send_response(204)
                self.end_headers()
                return
            self.reply(404, {"error": self.path})

    socketserver.ThreadingTCPServer.allow_reuse_address = True
    srv = socketserver.ThreadingTCPServer(("127.0.0.1", STUB_PORT), Handler)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv


# --- Icecast -----------------------------------------------------------------


def start_icecast(work):
    d = work / "icecast"
    (d / "log").mkdir(parents=True, exist_ok=True)
    share = pathlib.Path(ICECAST).resolve().parent.parent / "share" / "icecast"
    conf = d / "icecast.xml"
    conf.write_text(f"""<icecast>
  <limits><sources>2</sources></limits>
  <authentication>
    <source-password>testpass</source-password>
    <admin-user>admin</admin-user><admin-password>testpass</admin-password>
  </authentication>
  <hostname>localhost</hostname>
  <listen-socket><port>{ICECAST_PORT}</port><bind-address>127.0.0.1</bind-address></listen-socket>
  <paths>
    <logdir>{d / "log"}</logdir>
    <webroot>{share / "web"}</webroot>
    <adminroot>{share / "admin"}</adminroot>
  </paths>
  <logging><errorlog>error.log</errorlog><accesslog>access.log</accesslog><loglevel>2</loglevel></logging>
</icecast>
""")
    return subprocess.Popen([ICECAST, "-c", str(conf)],
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


# --- Liquidsoap --------------------------------------------------------------


class Liquidsoap:
    """One Liquidsoap process at a time, which a step can restart."""

    def __init__(self, liq, env, log):
        self.liq = liq
        self.env = env
        self.log = log
        self.proc = None

    def start(self):
        self.proc = subprocess.Popen(
            [LIQUIDSOAP, str(self.liq)],
            env=self.env, stdout=self.log, stderr=subprocess.STDOUT,
            start_new_session=True,
        )

    def stop(self):
        if self.proc is not None and self.proc.poll() is None:
            os.killpg(self.proc.pid, signal.SIGTERM)
            try:
                self.proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(self.proc.pid, signal.SIGKILL)
                self.proc.wait()

    def running(self):
        return self.proc.poll() is None


# --- Runner ------------------------------------------------------------------


def run_steps(sc, clock, liq, log):
    """Run the steps of a scenario in time order, then wait for its end."""
    steps = sorted(sc["steps"], key=lambda s: s[0])
    while clock.now() < sc["end"] and liq.running():
        if steps and clock.now() >= steps[0][0]:
            _, action, arg = steps.pop(0)
            if action == "jump":
                print(f"   {clock.now():%H:%M:%S}  jump to {arg:%H:%M:%S}", file=log, flush=True)
                clock.set(arg)
            elif action == "restart":
                print(f"   {clock.now():%H:%M:%S}  restart", file=log, flush=True)
                liq.stop()
                liq.start()
            continue
        time.sleep(0.2)


def judge(check, played):
    if check["check"] == "planned":
        starts = [planned for _, title, _, planned in played if title == check["title"]]
        return bool(starts) and all(s == check["planned"] for s in starts)
    hit = any(
        title == check["title"] and kind == check["kind"] and check["lo"] <= t <= check["hi"]
        for t, title, kind, _ in played
    )
    return hit == check["want"]


def describe(check):
    if check["check"] == "planned":
        return f"each post of '{check['title']}' has planned start {check['hms']} ({check['planned']})"
    verb = "airs" if check["want"] else "does not air"
    lo = f"{check['lo']:%H:%M:%S.%f}"[:-5]
    hi = f"{check['hi']:%H:%M:%S.%f}"[:-5]
    return f"{check['kind']} '{check['title']}' {verb} between {lo} and {hi}"


def run(liq_file, name, work, audio):
    """Run one scenario. Return 'pass', 'known', or 'fail'."""
    sc = SCENARIOS[name]
    clock = Clock(work / f"{name}.clock")
    stub = Stub(sc, audio, clock)
    clock.set(sc["start"])
    srv = serve(stub, audio)
    log_path = work / f"{name}.log"
    env = dict(
        os.environ,
        **clock.env(),
        API_BASE=f"http://localhost:{STUB_PORT}/api/playout",
        ICECAST_HOST="127.0.0.1",
        ICECAST_PORT=str(ICECAST_PORT),
        ICECAST_PASSWORD="testpass",
        ICECAST_MOUNT="/stream",
        PLAYOUT_SECRET="test-secret",
    )
    with open(log_path, "w") as log:
        liq = Liquidsoap(liq_file, env, log)
        liq.start()
        try:
            run_steps(sc, clock, liq, log)
        finally:
            liq.stop()
            srv.shutdown()
            srv.server_close()
        print("\n/now requests:", file=log)
        for t, path, code in stub.requests:
            print(f"   {t:%H:%M:%S.%f}  {code}  {path}", file=log)

    print(f"\n== {name}")
    for t, title, kind, planned in stub.played:
        print(f"   {t:%H:%M:%S.%f}  {kind:<12} {title}  (planned {planned})")
    result = "pass"
    for check in sc["checks"]:
        ok = judge(check, stub.played)
        issue = check["known_issue"]
        if issue is None:
            label = "PASS" if ok else "FAIL"
            if not ok:
                result = "fail"
        elif ok:
            label = "FIXED"
            result = "fail"
        else:
            label = "KNOWN"
            if result == "pass":
                result = "known"
        print(f"   {label}: {describe(check)}")
        if issue is not None:
            print(f"          Known issue: {issue}")
            if ok:
                print("          This check passes now. Remove its known_issue marker.")
    code = liq.proc.returncode
    if code not in (None, 0, -signal.SIGTERM):
        result = "fail"
        print(f"   FAIL: liquidsoap exited with code {code}")
    return result


def main():
    liq = pathlib.Path(sys.argv[1]).resolve()
    names = sys.argv[2:] or list(SCENARIOS)
    unknown = [n for n in names if n not in SCENARIOS]
    if unknown:
        sys.exit(f"Unknown scenario: {', '.join(unknown)}. Known: {', '.join(SCENARIOS)}")

    work = pathlib.Path(tempfile.mkdtemp(prefix="kpbj-liquidsoap-test-"))
    audio = Audio(work / "audio")
    ice = start_icecast(work)
    time.sleep(1)
    try:
        results = {n: run(liq, n, work, audio) for n in names}
    finally:
        ice.terminate()
        ice.wait()

    print()
    for n, r in results.items():
        print(f"{ {'pass': 'PASS ', 'known': 'KNOWN', 'fail': 'FAIL '}[r]}  {n}")
    if "fail" not in results.values():
        shutil.rmtree(work)
        sys.exit(0)
    print(f"\nLogs: {work}")
    sys.exit(1)


if __name__ == "__main__":
    main()
