# ──────────────────────────────────────────────────────────────
# pgBackRest — PostgreSQL backup with WAL archiving (PITR)
# ──────────────────────────────────────────────────────────────
#
# Daily full backups + continuous WAL archiving for point-in-time
# recovery. Backups stored locally on-VPS (repo1). An optional
# S3-compatible second repository (repo2) provides off-site
# disaster recovery. A oneshot service initialises the stanza
# after PostgreSQL starts, and a timer triggers the daily full
# backup.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.pgbackrest;
  pgCfg = config.services.postgresql;
  s3Enabled = cfg.s3 != null;
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

    s3 = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          bucket = lib.mkOption {
            type = lib.types.str;
            description = "S3 bucket name for off-site backups.";
          };
          endpoint = lib.mkOption {
            type = lib.types.str;
            description = "S3-compatible endpoint (e.g. sfo3.digitaloceanspaces.com).";
          };
          region = lib.mkOption {
            type = lib.types.str;
            description = "S3 region (e.g. sfo3).";
          };
          path = lib.mkOption {
            type = lib.types.str;
            default = "/pgbackrest";
            description = "Path prefix within the S3 bucket.";
          };
          retentionFull = lib.mkOption {
            type = lib.types.int;
            default = 14;
            description = "Number of full backups to retain in S3.";
          };
          secretsFile = lib.mkOption {
            type = lib.types.path;
            description = "Path to the SOPS-encrypted YAML containing aws_access_key_id and aws_secret_access_key.";
          };
        };
      });
      default = null;
      description = "Optional S3-compatible remote repository (repo2) for off-site backups.";
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
    '' + lib.optionalString s3Enabled ''
      repo2-type=s3
      repo2-s3-bucket=${cfg.s3.bucket}
      repo2-s3-endpoint=${cfg.s3.endpoint}
      repo2-s3-region=${cfg.s3.region}
      repo2-s3-uri-style=path
      repo2-path=${cfg.s3.path}
      repo2-retention-full=${toString cfg.s3.retentionFull}
    '' + ''

      [global:archive-push]
      compress-level=3
    '';

    # ── SOPS secrets for S3 credentials ────────────────────────
    sops.secrets = lib.mkIf s3Enabled {
      pgbackrest_s3_key = {
        sopsFile = cfg.s3.secretsFile;
        key = "aws_access_key_id_pgbackrest";
        owner = "postgres";
        restartUnits = [ "kpbj-pgbackrest-init.service" ];
      };
      pgbackrest_s3_key_secret = {
        sopsFile = cfg.s3.secretsFile;
        key = "aws_secret_access_key_pgbackrest";
        owner = "postgres";
        restartUnits = [ "kpbj-pgbackrest-init.service" ];
      };
    };

    sops.templates = lib.mkIf s3Enabled {
      "kpbj-pgbackrest-s3.env" = {
        content = ''
          PGBACKREST_REPO2_S3_KEY=${config.sops.placeholder.pgbackrest_s3_key}
          PGBACKREST_REPO2_S3_KEY_SECRET=${config.sops.placeholder.pgbackrest_s3_key_secret}
        '';
        owner = "postgres";
        group = "postgres";
        restartUnits = [ "postgresql.service" "kpbj-pgbackrest-init.service" "kpbj-backup-full.service" ];
      };
    };

    # ── PostgreSQL WAL archiving ───────────────────────────────
    services.postgresql.settings = {
      archive_mode = "on";
      archive_command = "${pkgs.pgbackrest}/bin/pgbackrest --stanza=${cfg.stanzaName} archive-push %p";
      wal_level = "replica";
    };

    # Allow archive_command (child of postgresql.service) to write
    # to the backup repo.  ProtectSystem=strict makes / read-only
    # for the service, so the repo path must be explicitly allowed.
    systemd.services.postgresql.serviceConfig.ReadWritePaths = [ cfg.repoPath ];

    # pgbackrest must be on PostgreSQL's PATH so that the restore_command
    # written by `pgbackrest restore` into postgresql.auto.conf can find
    # the binary during WAL recovery.
    systemd.services.postgresql.path = [ pkgs.pgbackrest ];

    # S3 credentials for archive_command (runs as child of postgresql.service)
    systemd.services.postgresql.serviceConfig.EnvironmentFile =
      lib.mkIf s3Enabled [ config.sops.templates."kpbj-pgbackrest-s3.env".path ];

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
        ExecStart =
          pkgs.writeShellScript "kpbj-pgbackrest-init" ''
            set -euo pipefail
            ${pkgs.pgbackrest}/bin/pgbackrest stanza-create --stanza=${cfg.stanzaName} || \
              ${pkgs.pgbackrest}/bin/pgbackrest stanza-upgrade --stanza=${cfg.stanzaName}
          '';
      } // lib.optionalAttrs s3Enabled {
        EnvironmentFile = [ config.sops.templates."kpbj-pgbackrest-s3.env".path ];
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
        ExecStart =
          if s3Enabled then
            pkgs.writeShellScript "kpbj-backup-full" ''
              set -euo pipefail
              ${pkgs.pgbackrest}/bin/pgbackrest backup --stanza=${cfg.stanzaName} --type=full --repo=1
              ${pkgs.pgbackrest}/bin/pgbackrest backup --stanza=${cfg.stanzaName} --type=full --repo=2 \
                || echo "WARNING: S3 backup (repo2) failed — local backup succeeded"
            ''
          else
            "${pkgs.pgbackrest}/bin/pgbackrest backup --stanza=${cfg.stanzaName} --type=full";
      } // lib.optionalAttrs s3Enabled {
        EnvironmentFile = [ config.sops.templates."kpbj-pgbackrest-s3.env".path ];
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
