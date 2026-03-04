# ──────────────────────────────────────────────────────────────
# Episode check job — daily systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the episode-check CLI as a oneshot systemd service on a
# timer. Sends reminder emails to hosts with missing episodes.
# Reads DATABASE_URL and SMTP config from the existing kpbj-web.env
# SOPS template.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, episode-check, ... }:

let
  cfg = config.kpbj.episodeCheck;
in
{
  options.kpbj.episodeCheck = {
    enable = lib.mkEnableOption "KPBJ episode check job";

    days = lib.mkOption {
      type = lib.types.listOf lib.types.int;
      default = [ 5 3 1 ];
      description = "List of days ahead to check for missing episodes (sends reminder at each threshold).";
    };

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Print what would be sent without sending emails.";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "Systemd calendar spec for how often to run the job.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-episode-check = {
      description = "KPBJ episode check — remind hosts about missing episodes";

      # Wait for postgresql to be available
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        # Reuse the existing kpbj-web.env which already contains DATABASE_URL
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart =
          let
            daysArgs = lib.concatMapStringsSep " " (d: "--days ${toString d}") cfg.days;
          in
          "${episode-check}/bin/episode-check ${daysArgs}"
          + lib.optionalString cfg.dryRun " --dry-run";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-episode-check = {
      description = "Run KPBJ episode check on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
