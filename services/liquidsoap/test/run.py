#!/usr/bin/env python3
"""Integration tests for services/liquidsoap/radio.liq.

The harness runs the real script with the pinned Liquidsoap binary. It starts a
stub playout API and a local Icecast. libfaketime sets the wall clock of
Liquidsoap to a few seconds before a schedule boundary, so the :58 break and
the :59:55 poll fire during the test. The stub records each /played POST, and
each scenario checks that record.

The clock runs at normal speed, and the harness jumps it forward over idle
time. A faster clock does not work. thread.when checks its condition every 0.5
seconds of real time, so at 2x or more it can step over the one-second windows
that radio.liq uses. A jump keeps those checks at normal speed.

Two rules apply to a jump:

  - Liquidsoap triggers inside the skipped time do not fire. Jump to a time
    before the boundary under test.
  - A jump fires every pending thread.run(delay=...) at once, because those
    delays use the wall clock. Do not jump in the first seconds after startup.

The audio clock uses the monotonic clock, which libfaketime leaves alone, so
the audio plays at normal speed across a jump.

Run it with `just test-liquidsoap`. The recipe supplies these variables:

  LIQUIDSOAP_BIN, FAKETIME_LIB, ICECAST_BIN, FFMPEG_BIN

Scenarios run one at a time, because radio.liq uses the fixed telnet port 1234.

A check can carry a known issue. Such a check describes the correct behavior,
and it fails today. Its failure is reported as KNOWN and does not fail the run.
When it passes, the run fails, so that the marker gets removed.

Usage: run.py LIQ_FILE [SCENARIO ...]
"""

import datetime as dt
import http.server
import json
import os
import pathlib
import shutil
import signal
import socket
import socketserver
import subprocess
import sys
import tempfile
import threading
import time

STUB_PORT = 18080
ICECAST_PORT = 18000
TELNET_PORT = 1234

LIQUIDSOAP = os.environ["LIQUIDSOAP_BIN"]
FAKETIME_LIB = os.environ["FAKETIME_LIB"]
ICECAST = os.environ["ICECAST_BIN"]
FFMPEG = os.environ["FFMPEG_BIN"]

DAY = dt.date(2026, 10, 21)


def at(hms):
    """Return a fake local time on the test day."""
    h, m, s = (int(x) for x in hms.split(":"))
    return dt.datetime.combine(DAY, dt.time(h, m, s))


# --- Checks and steps --------------------------------------------------------


def airs(title, kind, lo, hi, known_issue=None):
    """The track must appear in /played between lo and hi."""
    return dict(want=True, title=title, kind=kind, lo=at(lo), hi=at(hi), known_issue=known_issue)


def absent(title, kind, lo, hi, known_issue=None):
    """The track must not appear in /played between lo and hi."""
    return dict(want=False, title=title, kind=kind, lo=at(lo), hi=at(hi), known_issue=known_issue)


def jump(when, to):
    """At fake time `when`, move the clock forward to `to`."""
    return (at(when), "jump", at(to))


def telnet(when, command):
    """At fake time `when`, send one command to the Liquidsoap telnet server."""
    return (at(when), "telnet", command)


def force_play(when, title):
    """At fake time `when`, force-play a track from the scenario, as the dashboard does."""
    return (at(when), "force_play", title)


BREAK_DURING_DOWNLOAD = (
    "A break does not drop an episode that is still downloading. The episode "
    "airs after the break, and the next show starts late. Do not restart "
    "Liquidsoap in the 30 seconds before :28 or :58."
)

# --- Scenarios ---------------------------------------------------------------
#
# start:   fake time when Liquidsoap starts. Startup takes about 5 seconds.
# end:     fake time when the run stops.
# slots:   (start, end, title, audio length in seconds). /now returns the slot
#          that is on air 10 seconds from now, as the real API does. It also
#          returns the offset into that slot, so a scenario that starts in a
#          slot needs audio that lasts past that offset.
# tracks:  extra episode files, for force_play. {title: audio length}
# filler:  audio length of the filler track. The default is 600 seconds.
# slow_audio: {title: seconds} the stub waits before it sends the audio of a
#          track, as slow storage does. The title "Station ID" delays the
#          break track.
# now_null: (from, to) fake times when /now returns null, as it does on a
#          database error.
# breaks:  fake times when /break returns a station ID. At other times it
#          returns an empty list.
# break_secs: audio length of the station ID. The default is 5 seconds.
# steps:   jump(...), telnet(...), and force_play(...) steps.
# checks:  airs(...) and absent(...) checks on the /played record.

SCENARIOS = {
    # Show A ends before the :58 break, so the scheduled queue is idle when the
    # break comes. Show B must still air at 17:00.
    "episode-ends-before-break": dict(
        start=at("16:57:10"),
        end=at("17:00:15"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1650),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        breaks=[at("16:58:00")],
        steps=[jump("16:58:15", "16:59:40")],
        checks=[airs("Show B", "episode", "16:59:55", "17:00:15")],
    ),
    # Show A is still on air at the :58 break, so the break cuts it. Show B
    # must air at 17:00, and Show A must not come back after the break. The
    # break is a 5 second station ID, so it ends at about 16:58:04. Filler must
    # start then. If the buffer still holds Show A, its audio plays first and
    # filler starts at about 16:58:07. Issue #156.
    "episode-cut-by-break": dict(
        start=at("16:57:10"),
        end=at("17:00:15"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1800),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        breaks=[at("16:58:00")],
        steps=[jump("16:58:15", "16:59:40")],
        checks=[
            airs("Show B", "episode", "16:59:55", "17:00:15"),
            absent("Show A", "episode", "16:58:01", "16:58:15"),
            airs("Filler", "ephemeral", "16:58:01", "16:58:05"),
        ],
    ),
    # The break cuts Show A and runs for 120 seconds, the longest that four
    # break items of 30 seconds make. The 16:59:55 poll then pushes Show B
    # while the break is on air. Show B must start when the break ends, with no
    # Show A and no filler before it. Issue #156.
    "long-break-cut-episode": dict(
        start=at("16:57:10"),
        end=at("17:00:30"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1800),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        breaks=[at("16:58:00")],
        break_secs=120,
        steps=[],
        checks=[
            absent("Show A", "episode", "16:58:01", "17:00:30"),
            airs("Show B", "episode", "16:59:58", "17:00:10"),
            absent("Filler", "ephemeral", "16:59:58", "17:00:30"),
        ],
    ),
    # The break cuts Show A, and the station ID takes 8 seconds to download.
    # The buffer holds 5 seconds of Show A, so it runs dry before the break
    # starts, and fallback() gets no Show A metadata to replay. Show A must not
    # air after the break, and Show B must air at 17:00. Issue #156.
    "break-download-drains-buffer": dict(
        start=at("16:57:10"),
        end=at("17:00:15"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1800),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        slow_audio={"Station ID": 8},
        breaks=[at("16:58:00")],
        steps=[jump("16:58:40", "16:59:40")],
        checks=[
            airs("Station ID", "station_id", "16:58:06", "16:58:12"),
            absent("Show A", "episode", "16:58:09", "16:58:40"),
            airs("Show B", "episode", "16:59:55", "17:00:15"),
        ],
    ),
    # As above, the buffer runs dry before the break, so the replay of Show A
    # never comes. A staff member then force-plays Show A. The forced episode
    # is a new request, so it must air and post, though it has the same URL as
    # the cut episode. Issue #156.
    "force-play-cut-episode": dict(
        start=at("16:57:10"),
        end=at("16:58:45"),
        slots=[(at("16:30:00"), at("17:00:00"), "Show A", 1800)],
        slow_audio={"Station ID": 8},
        breaks=[at("16:58:00")],
        steps=[force_play("16:58:25", "Show A")],
        checks=[airs("Show A", "episode", "16:58:25", "16:58:35")],
    ),
    # The last show of the night ends, and two breaks come with an idle queue,
    # as the hourly breaks do overnight. The next show must still air.
    "breaks-with-idle-queue": dict(
        start=at("16:27:10"),
        end=at("17:00:15"),
        slots=[
            (at("16:00:00"), at("16:30:00"), "Late Show", 1650),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        breaks=[at("16:28:00"), at("16:58:00")],
        steps=[
            jump("16:28:15", "16:57:40"),
            jump("16:58:15", "16:59:40"),
        ],
        checks=[airs("Show B", "episode", "16:59:55", "17:00:15")],
    ),
    # The dashboard skip-track command runs with nothing scheduled. The next
    # show must still air.
    "skip-track-with-idle-queue": dict(
        start=at("16:59:05"),
        end=at("17:00:15"),
        slots=[(at("17:00:00"), at("18:00:00"), "Show B", 120)],
        breaks=[],
        steps=[telnet("16:59:30", "skip_track")],
        checks=[airs("Show B", "episode", "16:59:55", "17:00:15")],
    ),
    # A staff member skips an episode early in a one-hour slot. The :29:55 poll
    # gets the same episode from /now. The episode must stay skipped. Issue #159.
    "skip-track-mid-slot": dict(
        start=at("16:14:50"),
        end=at("16:30:20"),
        slots=[(at("16:00:00"), at("17:00:00"), "Show A", 3600)],
        breaks=[],
        steps=[telnet("16:15:15", "skip_track"), jump("16:15:25", "16:29:40")],
        checks=[
            airs("Show A", "episode", "16:14:50", "16:15:15"),
            absent("Show A", "episode", "16:29:50", "16:30:20"),
        ],
    ),
    # Liquidsoap restarts after the :58 break, in the last minutes of Show A. The
    # audio has the longest length that a 30-minute slot accepts, so it has ended
    # by then. The next show must air at 17:00.
    "restart-near-end-of-show": dict(
        start=at("16:58:20"),
        end=at("17:00:20"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1680),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        breaks=[],
        steps=[],
        checks=[airs("Show B", "episode", "16:59:55", "17:00:20")],
    ),
    # Liquidsoap restarts just before the :58 break, and Show A is still
    # downloading when the break comes. The break must drop Show A, and Show B
    # must air at 17:00. Issue #168.
    "break-during-download": dict(
        start=at("16:57:47"),
        end=at("17:00:40"),
        slots=[
            (at("16:30:00"), at("17:00:00"), "Show A", 1800),
            (at("17:00:00"), at("18:00:00"), "Show B", 120),
        ],
        slow_audio={"Show A": 15},
        breaks=[at("16:58:00")],
        steps=[],
        checks=[
            absent("Show A", "episode", "16:57:47", "17:00:40", known_issue=BREAK_DURING_DOWNLOAD),
            airs("Show B", "episode", "16:59:55", "17:00:15", known_issue=BREAK_DURING_DOWNLOAD),
        ],
    ),
    # Liquidsoap restarts 20 minutes into Show A, and the audio has less than a
    # minute left at that point. Filler can start only if the episode starts at
    # its offset. From 0:00, the episode would play for 20 minutes. Issue #152.
    "restart-mid-show": dict(
        start=at("16:50:10"),
        end=at("16:51:10"),
        slots=[(at("16:30:00"), at("17:00:00"), "Show A", 1240)],
        breaks=[],
        steps=[],
        checks=[
            airs("Show A", "episode", "16:50:10", "16:50:30"),
            airs("Filler", "ephemeral", "16:50:30", "16:51:05"),
        ],
    ),
    # Liquidsoap restarts after the audio of Show A has ended. The stream must
    # stay on filler. An episode with no stored duration gets here, because /now
    # can not tell that the audio has ended. fallback() does not pull from an
    # unselected source, so the second filler track airs only if the stream did
    # not stay on the empty episode. Issue #152.
    "restart-after-audio-ends": dict(
        start=at("16:50:10"),
        end=at("16:50:50"),
        slots=[(at("16:30:00"), at("17:00:00"), "Show A", 600)],
        filler=20,
        breaks=[],
        steps=[],
        checks=[
            absent("Show A", "episode", "16:50:10", "16:50:50"),
            airs("Filler", "ephemeral", "16:50:25", "16:50:50"),
        ],
    ),
    # The :59:55 poll gets null because of a database error. The poll after the
    # boundary must still put the show on air, near its start.
    "boundary-poll-fails": dict(
        start=at("16:59:05"),
        end=at("17:00:25"),
        slots=[(at("17:00:00"), at("18:00:00"), "Show B", 120)],
        now_null=(at("16:59:50"), at("17:00:00")),
        breaks=[],
        steps=[],
        checks=[airs("Show B", "episode", "17:00:05", "17:00:20")],
    ),
    # A database error makes both polls at the 16:30 boundary get null. The two
    # polls must not count as two empty polls, because that clears the record of
    # the episode. A staff member then skips the episode, and the 16:59:55 poll
    # would air it again.
    "retry-keeps-last-scheduled": dict(
        start=at("16:29:30"),
        end=at("17:00:20"),
        slots=[(at("16:00:00"), at("18:00:00"), "Show A", 7200)],
        now_null=(at("16:29:50"), at("16:30:10")),
        breaks=[],
        steps=[telnet("16:30:12", "skip_track"), jump("16:30:20", "16:59:40")],
        checks=[absent("Show A", "episode", "16:59:50", "17:00:20")],
    ),
    # A staff member force-plays an episode while another one is on air. The
    # forced episode must start at once.
    "force-play-during-episode": dict(
        start=at("16:50:10"),
        end=at("16:51:00"),
        slots=[(at("16:30:00"), at("17:00:00"), "Show A", 1800)],
        tracks={"Show F": 120},
        breaks=[],
        steps=[force_play("16:50:35", "Show F")],
        checks=[airs("Show F", "episode", "16:50:35", "16:50:50")],
    ),
    # A staff member force-plays the wrong episode, then force-plays the
    # correct one to fix it. The correct episode must replace the wrong one at
    # once. Issue #157.
    "force-play-twice": dict(
        start=at("16:50:10"),
        end=at("16:51:15"),
        slots=[(at("16:30:00"), at("17:00:00"), "Show A", 1800)],
        tracks={"Show F": 120, "Show G": 120},
        breaks=[],
        steps=[force_play("16:50:30", "Show F"), force_play("16:50:45", "Show G")],
        checks=[
            airs("Show F", "episode", "16:50:30", "16:50:45"),
            airs("Show G", "episode", "16:50:45", "16:51:00"),
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
        self.lock = threading.Lock()
        self.urls = {}
        tracks = [(title, secs) for _, _, title, secs in sc["slots"]]
        tracks += list(sc.get("tracks", {}).items())
        for i, (title, secs) in enumerate(tracks):
            self.urls[title] = audio.tone(slug(title, secs), secs, 440 + 110 * i)
        filler_secs = sc.get("filler", 600)
        self.filler = audio.tone(f"filler-{filler_secs}s", filler_secs, 220)
        break_secs = sc.get("break_secs", 5)
        self.station_id = audio.tone(f"station-id-{break_secs}s", break_secs, 880)
        files = {**self.urls, "Station ID": self.station_id}
        self.slow_audio = {
            files[title].rsplit("/", 1)[-1]: delay
            for title, delay in sc.get("slow_audio", {}).items()
        }

    def now(self):
        real = self.clock.now()
        fault = self.sc.get("now_null")
        if fault and fault[0] <= real < fault[1]:
            return None
        t = real + dt.timedelta(seconds=10)
        for start, end, title, _ in self.sc["slots"]:
            if start <= t < end:
                offset = max(0, int((real - start).total_seconds()))
                return {"url": self.urls[title], "title": title, "artist": "Test",
                        "offset_seconds": offset}
        return None

    def brk(self):
        t = self.clock.now()
        for b in self.sc["breaks"]:
            if abs((t - b).total_seconds()) < 5:
                return [{"url": self.station_id, "title": "Station ID",
                         "artist": "KPBJ", "source_type": "station_id"}]
        return []

    def fallback(self):
        return [{"url": self.filler, "title": "Filler", "artist": "KPBJ",
                 "source_type": "ephemeral"}]

    def record(self, body):
        with self.lock:
            self.played.append((self.clock.now(), body.get("prTitle"), body.get("prSourceType")))


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
            p = self.path
            if p.startswith("/audio/"):
                name = p.rsplit("/", 1)[-1]
                time.sleep(stub.slow_audio.get(name, 0))
                return self.reply(200, (audio.root / name).read_bytes(), "audio/mpeg")
            if p.endswith("/now"):
                return self.reply(200, stub.now())
            if p.endswith("/break"):
                return self.reply(200, stub.brk())
            if p.endswith("/fallback"):
                return self.reply(200, stub.fallback())
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


# --- Telnet ------------------------------------------------------------------


def send_telnet(command):
    """Send one command, read the answer up to END, and close the session."""
    with socket.create_connection(("127.0.0.1", TELNET_PORT), timeout=10) as s:
        s.sendall(command.encode() + b"\n")
        answer = b""
        while b"END" not in answer:
            chunk = s.recv(4096)
            if not chunk:
                break
            answer += chunk
        s.sendall(b"quit\n")
    return answer.decode(errors="replace").replace("END", "").strip()


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


# --- Runner ------------------------------------------------------------------


def run_steps(sc, stub, clock, proc, log):
    """Run the steps of a scenario in time order, then wait for its end."""
    steps = sorted(sc["steps"], key=lambda s: s[0])
    while clock.now() < sc["end"] and proc.poll() is None:
        if steps and clock.now() >= steps[0][0]:
            _, action, arg = steps.pop(0)
            if action == "jump":
                print(f"   {clock.now():%H:%M:%S}  jump to {arg:%H:%M:%S}", file=log, flush=True)
                clock.set(arg)
            else:
                if action == "force_play":
                    payload = {"url": stub.urls[arg], "title": arg, "artist": "Test"}
                    command = f"force_play {json.dumps(payload)}"
                else:
                    command = arg
                answer = send_telnet(command)
                print(f"   {clock.now():%H:%M:%S}  telnet {command} -> {answer}", file=log, flush=True)
            continue
        time.sleep(0.2)


def judge(check, played):
    hit = any(
        title == check["title"] and kind == check["kind"] and check["lo"] <= t <= check["hi"]
        for t, title, kind in played
    )
    return hit == check["want"]


def describe(check):
    verb = "airs" if check["want"] else "does not air"
    return f"{check['kind']} '{check['title']}' {verb} between {check['lo']:%H:%M:%S} and {check['hi']:%H:%M:%S}"


def run(liq, name, work, audio):
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
        proc = subprocess.Popen(
            [LIQUIDSOAP, str(liq)],
            env=env, stdout=log, stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        try:
            run_steps(sc, stub, clock, proc, log)
        finally:
            if proc.poll() is None:
                os.killpg(proc.pid, signal.SIGTERM)
                try:
                    proc.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(proc.pid, signal.SIGKILL)
            srv.shutdown()
            srv.server_close()

    print(f"\n== {name}")
    for t, title, kind in stub.played:
        print(f"   {t:%H:%M:%S}  {kind:<10} {title}")
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
    if proc.returncode not in (None, 0, -signal.SIGTERM):
        result = "fail"
        print(f"   FAIL: liquidsoap exited with code {proc.returncode}")
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
