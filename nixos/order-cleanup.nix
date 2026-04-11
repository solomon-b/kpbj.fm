# ──────────────────────────────────────────────────────────────
# Order cleanup job — systemd timer (every 30 minutes)
# ──────────────────────────────────────────────────────────────
#
# Cancels stale pending orders (>30 min) and restores their
# held inventory. Reads DATABASE_URL from kpbj-web.env SOPS
# template.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, order-cleanup, ... }:

let
  cfg = config.kpbj.orderCleanup;
in
{
  options.kpbj.orderCleanup = {
    enable = lib.mkEnableOption "KPBJ order cleanup job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (print what would be cancelled without cancelling).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*:0/30";
      description = "Systemd calendar spec for how often to run the job.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-order-cleanup = {
      description = "KPBJ stale pending order cleanup";

      # Wait for postgresql to be available
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        # Reuse the existing kpbj-web.env which already contains DATABASE_URL
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart = "${order-cleanup}/bin/order-cleanup"
          + lib.optionalString cfg.dryRun " --dry-run";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-order-cleanup = {
      description = "Run KPBJ order cleanup on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
