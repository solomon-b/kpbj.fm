# ──────────────────────────────────────────────────────────────
# Token cleanup job — hourly systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the token-cleanup CLI as a oneshot systemd service on a
# timer. Deletes expired authentication tokens from the database.
# Reads DATABASE_URL from the existing kpbj-web.env SOPS template.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, token-cleanup, ... }:

let
  cfg = config.kpbj.tokenCleanup;
in
{
  options.kpbj.tokenCleanup = {
    enable = lib.mkEnableOption "KPBJ token cleanup job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (print what would be deleted without deleting).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "hourly";
      description = "Systemd calendar spec for how often to run the job.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-token-cleanup = {
      description = "KPBJ expired token cleanup";

      # Wait for postgresql to be available
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        # Reuse the existing kpbj-web.env which already contains DATABASE_URL
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart = "${token-cleanup}/bin/token-cleanup"
          + lib.optionalString cfg.dryRun " --dry-run";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-token-cleanup = {
      description = "Run KPBJ token cleanup on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
