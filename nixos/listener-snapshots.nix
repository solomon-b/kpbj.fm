# ──────────────────────────────────────────────────────────────
# Listener snapshot poller — systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the listener-snapshots CLI as a oneshot systemd service on
# a timer. Polls Icecast for the current listener count and
# records it in the database. Reads DATABASE_URL from the
# existing kpbj-web.env SOPS template.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, listener-snapshots, ... }:

let
  cfg = config.kpbj.listenerSnapshots;
in
{
  options.kpbj.listenerSnapshots = {
    enable = lib.mkEnableOption "KPBJ listener snapshot poller";

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*:*:00";
      description = "Systemd calendar spec for polling frequency (default: every minute).";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-listener-snapshots = {
      description = "KPBJ listener snapshot poller";

      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      environment = {
        ICECAST_METADATA_URL = "http://127.0.0.1:${toString config.kpbj.streaming.icecastPort}/status-json.xsl";
      };

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart = "${listener-snapshots}/bin/listener-snapshots";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-listener-snapshots = {
      description = "Run KPBJ listener snapshot poller on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        AccuracySec = "1s";
        Persistent = true;
      };
    };
  };
}
