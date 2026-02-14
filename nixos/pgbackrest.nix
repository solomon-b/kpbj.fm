# ──────────────────────────────────────────────────────────────
# pgBackRest — PostgreSQL backup with WAL archiving (PITR)
# ──────────────────────────────────────────────────────────────
#
# Daily full backups + continuous WAL archiving for point-in-time
# recovery. Backups stored locally on-VPS. A oneshot service
# initialises the stanza after PostgreSQL starts, and a timer
# triggers the daily full backup.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.pgbackrest;
  pgCfg = config.services.postgresql;
in
{
  options.kpbj.pgbackrest = {
    enable = lib.mkEnableOption "KPBJ pgBackRest PostgreSQL backups";

    stanzaName = lib.mkOption {
      type = lib.types.str;
      default = "kpbj";
      description = "pgBackRest stanza name.";
    };

    repoPath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/pgbackrest";
      description = "Local path for the pgBackRest backup repository.";
    };

    retentionFull = lib.mkOption {
      type = lib.types.int;
      default = 14;
      description = "Number of full backups to retain.";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "Systemd calendar spec for backup frequency.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── pgbackrest package ─────────────────────────────────────
    environment.systemPackages = [ pkgs.pgbackrest ];

    # ── pgbackrest.conf ────────────────────────────────────────
    environment.etc."pgbackrest/pgbackrest.conf".text = ''
      [${cfg.stanzaName}]
      pg1-path=${pgCfg.dataDir}

      [global]
      repo1-path=${cfg.repoPath}
      repo1-retention-full=${toString cfg.retentionFull}
      log-level-console=info
      log-level-file=detail
      start-fast=y
      delta=y
      compress-type=zst

      [global:archive-push]
      compress-level=3
    '';

    # ── PostgreSQL WAL archiving ───────────────────────────────
    services.postgresql.settings = {
      archive_mode = "on";
      archive_command = "${pkgs.pgbackrest}/bin/pgbackrest --stanza=${cfg.stanzaName} archive-push %p";
      wal_level = "replica";
    };

    # ── Repo + log directories ───────────────────────────────────
    systemd.tmpfiles.rules = [
      "d ${cfg.repoPath} 0750 postgres postgres -"
      "d /var/log/pgbackrest 0750 postgres postgres -"
    ];

    # ── Stanza init (oneshot) ──────────────────────────────────
    # Creates the stanza on first run, upgrades if it already exists.
    systemd.services.kpbj-pgbackrest-init = {
      description = "Initialise pgBackRest stanza for KPBJ";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "postgres";
        Group = "postgres";
        ExecStart = pkgs.writeShellScript "kpbj-pgbackrest-init" ''
          set -euo pipefail
          ${pkgs.pgbackrest}/bin/pgbackrest stanza-create --stanza=${cfg.stanzaName} || \
            ${pkgs.pgbackrest}/bin/pgbackrest stanza-upgrade --stanza=${cfg.stanzaName}
        '';
      };
    };

    # ── Backup service (oneshot) ───────────────────────────────
    systemd.services.kpbj-backup-full = {
      description = "pgBackRest full backup for KPBJ";
      after = [ "kpbj-pgbackrest-init.service" ];
      requires = [ "kpbj-pgbackrest-init.service" ];

      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        Group = "postgres";
        ExecStart = "${pkgs.pgbackrest}/bin/pgbackrest backup --stanza=${cfg.stanzaName} --type=full";
      };
    };

    # ── Backup timer ───────────────────────────────────────────
    systemd.timers.kpbj-backup-full = {
      description = "Daily pgBackRest full backup for KPBJ";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
