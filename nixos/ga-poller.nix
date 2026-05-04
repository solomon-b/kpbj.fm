# ──────────────────────────────────────────────────────────────
# Google Analytics poller — systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the ga-poller CLI as a oneshot systemd service on a daily
# timer. Calls the GA Data API for top referrers, countries, and
# cities and records them in ga_snapshots. Reads DATABASE_URL,
# GA_PROPERTY_ID, and GA_SERVICE_ACCOUNT_KEY_PATH from the
# kpbj-ga-poller.env SOPS template (rendered in web.nix).
#
# GA's runReport endpoint resolves to whole UTC days, so polling
# is daily — not hourly. The default schedule fires shortly after
# midnight UTC to capture the previous day.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ga-poller, ... }:

let
  cfg = config.kpbj.gaPoller;
in
{
  options.kpbj.gaPoller = {
    enable = lib.mkEnableOption "KPBJ Google Analytics poller";

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 01:30:00";
      description = "Systemd calendar spec for polling frequency (default: daily at 01:30 UTC).";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-ga-poller = {
      description = "KPBJ Google Analytics poller";

      after = [ "postgresql.service" "network-online.target" ];
      requires = [ "postgresql.service" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        RuntimeDirectory = "kpbj-ga-poller";
        RuntimeDirectoryMode = "0700";

        EnvironmentFile = [ config.sops.templates."kpbj-ga-poller.env".path ];

        # The '+' prefix runs ExecStartPre as root so it can read the
        # sops secret file, then copies it to RuntimeDirectory where
        # the DynamicUser can read it. Same pattern as sync-host-emails.
        ExecStartPre = "+" + pkgs.writeShellScript "kpbj-ga-poller-setup" ''
          set -euo pipefail
          dir=/run/kpbj-ga-poller
          cp ${config.sops.secrets.google_analytics_service_account_key.path} "$dir/sa-key.json"
          chown "$(stat -c %u "$dir")" "$dir"/*
        '';

        ExecStart = "${ga-poller}/bin/ga-poller";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-ga-poller = {
      description = "Run KPBJ Google Analytics poller on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        AccuracySec = "1m";
        Persistent = true;
      };
    };
  };
}
