# ──────────────────────────────────────────────────────────────
# Schedule calendar sync job: systemd timer, every 5 minutes
# ──────────────────────────────────────────────────────────────
#
# Runs the sync-schedule-calendar CLI as a oneshot systemd service
# on a timer. The job makes one Google Calendar equal to the
# broadcast schedule. It reads the service account credentials and
# the calendar ID from SOPS secrets. Staging runs in --dry-run mode.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, sync-schedule-calendar, ... }:

let
  cfg = config.kpbj.syncScheduleCalendar;
in
{
  options.kpbj.syncScheduleCalendar = {
    enable = lib.mkEnableOption "KPBJ schedule calendar sync job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (no side effects).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*:0/5";
      description = "Systemd calendar spec for how often to run the job.";
    };

    baseUrl = lib.mkOption {
      type = lib.types.str;
      description = "Website base URL for the show links in the events, with no slash at the end.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── SOPS secrets ────────────────────────────────────────────
    sops.secrets.google_calendar_sa_email = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-schedule-calendar.service" ];
    };
    sops.secrets.google_calendar_sa_private_key = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-schedule-calendar.service" ];
    };
    sops.secrets.google_calendar_id = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-schedule-calendar.service" ];
    };

    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-sync-schedule-calendar = {
      description = "KPBJ schedule calendar sync";

      # Wait for postgresql to be available
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        RuntimeDirectory = "kpbj-sync-schedule-calendar";
        RuntimeDirectoryMode = "0700";

        # Reuse the existing kpbj-web.env which contains DATABASE_URL
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        # The '+' prefix runs ExecStartPre as root so it can read
        # sops secret files, then copies them to RuntimeDirectory
        # where the DynamicUser can read them.
        ExecStartPre = "+" + pkgs.writeShellScript "kpbj-sync-schedule-calendar-setup" ''
          set -euo pipefail
          dir=/run/kpbj-sync-schedule-calendar
          cp ${config.sops.secrets.google_calendar_sa_email.path} "$dir/sa-email"
          cp ${config.sops.secrets.google_calendar_sa_private_key.path} "$dir/sa-private-key"
          cp ${config.sops.secrets.google_calendar_id.path} "$dir/calendar-id"

          # Chown to the DynamicUser so ExecStart can read the secrets.
          # The RuntimeDirectory (0700) already restricts access.
          chown "$(stat -c %u "$dir")" "$dir"/*
        '';

        ExecStart = pkgs.writeShellScript "kpbj-sync-schedule-calendar-run" ''
          set -euo pipefail
          dir=/run/kpbj-sync-schedule-calendar
          exec ${sync-schedule-calendar}/bin/sync-schedule-calendar \
            --sa-email "$(cat "$dir/sa-email")" \
            --sa-private-key "$(cat "$dir/sa-private-key")" \
            --calendar-id "$(cat "$dir/calendar-id")" \
            --base-url ${lib.escapeShellArg cfg.baseUrl} \
            ${lib.optionalString cfg.dryRun "--dry-run"}
        '';

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    # No Persistent setting. A run that the host missed needs no
    # catch-up, because the next run does the same work.
    systemd.timers.kpbj-sync-schedule-calendar = {
      description = "Run KPBJ schedule calendar sync on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
      };
    };
  };
}
