# ──────────────────────────────────────────────────────────────
# Host email sync job — hourly systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the sync-host-emails CLI as a oneshot systemd service on a
# timer. Reads Google service account credentials from SOPS
# secrets. Staging runs in --dry-run mode.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, sync-host-emails, ... }:

let
  cfg = config.kpbj.syncHostEmails;
in
{
  options.kpbj.syncHostEmails = {
    enable = lib.mkEnableOption "KPBJ host email sync job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (no side effects).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "hourly";
      description = "Systemd calendar spec for how often to run the job.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── SOPS secrets ────────────────────────────────────────────
    sops.secrets.google_sa_email = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-host-emails.service" ];
    };
    sops.secrets.google_sa_private_key = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-host-emails.service" ];
    };
    sops.secrets.google_delegated_user = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-host-emails.service" ];
    };
    sops.secrets.google_group_email = {
      sopsFile = ../secrets/google.yaml;
      restartUnits = [ "kpbj-sync-host-emails.service" ];
    };

    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-sync-host-emails = {
      description = "KPBJ host email sync";

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        RuntimeDirectory = "kpbj-sync-host-emails";
        RuntimeDirectoryMode = "0700";

        # The '+' prefix runs ExecStartPre as root so it can read
        # sops secret files, then copies them to RuntimeDirectory
        # where the DynamicUser can read them.
        ExecStartPre = "+" + pkgs.writeShellScript "kpbj-sync-host-emails-setup" ''
          set -euo pipefail
          dir=/run/kpbj-sync-host-emails
          cp ${config.sops.secrets.google_sa_email.path} "$dir/sa-email"
          cp ${config.sops.secrets.google_sa_private_key.path} "$dir/sa-private-key"
          cp ${config.sops.secrets.google_delegated_user.path} "$dir/delegated-user"
          cp ${config.sops.secrets.google_group_email.path} "$dir/group-email"

          # Chown to the DynamicUser so ExecStart can read the secrets.
          # The RuntimeDirectory (0700) already restricts access.
          chown "$(stat -c %u "$dir")" "$dir"/*
        '';

        ExecStart = pkgs.writeShellScript "kpbj-sync-host-emails-run" ''
          set -euo pipefail
          dir=/run/kpbj-sync-host-emails
          exec ${sync-host-emails}/bin/sync-host-emails \
            --sa-email "$(cat "$dir/sa-email")" \
            --sa-private-key "$(cat "$dir/sa-private-key")" \
            --delegated-user "$(cat "$dir/delegated-user")" \
            --group-email "$(cat "$dir/group-email")" \
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
    systemd.timers.kpbj-sync-host-emails = {
      description = "Run KPBJ host email sync on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
