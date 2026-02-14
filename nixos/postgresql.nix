# ──────────────────────────────────────────────────────────────
# PostgreSQL — local database for the KPBJ web service
# ──────────────────────────────────────────────────────────────
#
# Each host MUST set pgPackage explicitly to prevent accidental
# major-version upgrades (which destroy the data directory).
# A oneshot service sets the DB user password from a file
# (typically a SOPS secret) after PostgreSQL starts.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.postgresql;
in
{
  options.kpbj.postgresql = {
    enable = lib.mkEnableOption "KPBJ PostgreSQL database";

    pgPackage = lib.mkOption {
      type = lib.types.package;
      description = "PostgreSQL package to use. Each host must pin this explicitly to prevent accidental major-version upgrades.";
    };

    dbName = lib.mkOption {
      type = lib.types.str;
      default = "kpbj_fm";
      description = "Name of the PostgreSQL database.";
    };

    dbUser = lib.mkOption {
      type = lib.types.str;
      default = "kpbj";
      description = "PostgreSQL user for the web service.";
    };

    passwordFile = lib.mkOption {
      type = lib.types.path;
      description = "File containing the database user password (e.g. a SOPS secret path).";
    };

    readonlyPasswordFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "File containing the read-only DB role password.";
    };

    readwritePasswordFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "File containing the read-write DB role password.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── PostgreSQL service ───────────────────────────────────────
    services.postgresql = {
      enable = true;
      package = cfg.pgPackage;

      ensureDatabases = [ cfg.dbName ];
      ensureUsers = [
        {
          name = cfg.dbUser;
          ensureDBOwnership = true;
        }
        { name = "kpbj_readonly"; }
        { name = "kpbj_readwrite"; }
      ];

      # Allow MD5 (password) authentication for TCP connections on localhost.
      # The Haskell app connects via TCP, not Unix sockets.
      authentication = lib.mkForce ''
        # TYPE  DATABASE        USER            ADDRESS                 METHOD
        local   all             all                                     peer
        host    all             all             127.0.0.1/32            md5
        host    all             all             ::1/128                 md5
      '';
    };

    # ── Password setup (oneshot) ─────────────────────────────────
    # Sets the DB user password from a file after PostgreSQL starts.
    # RemainAfterExit ensures dependent services see this as "active".
    systemd.services.kpbj-postgres-setup = {
      description = "Set KPBJ PostgreSQL user password";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "kpbj-postgres-setup" ''
          set -euo pipefail
          PSQL="${pkgs.util-linux}/bin/runuser -u postgres -- ${config.services.postgresql.package}/bin/psql"

          PASSWORD=$(cat ${cfg.passwordFile})
          $PSQL -c "ALTER USER ${cfg.dbUser} PASSWORD '$PASSWORD';"

          ${lib.optionalString (cfg.readonlyPasswordFile != null) ''
            RO_PASSWORD=$(cat ${cfg.readonlyPasswordFile})
            $PSQL -c "ALTER USER kpbj_readonly PASSWORD '$RO_PASSWORD';"
            $PSQL -d ${cfg.dbName} -c "
              GRANT CONNECT ON DATABASE ${cfg.dbName} TO kpbj_readonly;
              GRANT USAGE ON SCHEMA public TO kpbj_readonly;
              GRANT SELECT ON ALL TABLES IN SCHEMA public TO kpbj_readonly;
              GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO kpbj_readonly;
              ALTER DEFAULT PRIVILEGES FOR ROLE ${cfg.dbUser} IN SCHEMA public GRANT SELECT ON TABLES TO kpbj_readonly;
              ALTER DEFAULT PRIVILEGES FOR ROLE ${cfg.dbUser} IN SCHEMA public GRANT SELECT ON SEQUENCES TO kpbj_readonly;
            "
          ''}

          ${lib.optionalString (cfg.readwritePasswordFile != null) ''
            RW_PASSWORD=$(cat ${cfg.readwritePasswordFile})
            $PSQL -c "ALTER USER kpbj_readwrite PASSWORD '$RW_PASSWORD';"
            $PSQL -d ${cfg.dbName} -c "
              GRANT CONNECT ON DATABASE ${cfg.dbName} TO kpbj_readwrite;
              GRANT USAGE ON SCHEMA public TO kpbj_readwrite;
              GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO kpbj_readwrite;
              GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO kpbj_readwrite;
              ALTER DEFAULT PRIVILEGES FOR ROLE ${cfg.dbUser} IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO kpbj_readwrite;
              ALTER DEFAULT PRIVILEGES FOR ROLE ${cfg.dbUser} IN SCHEMA public GRANT USAGE, SELECT ON SEQUENCES TO kpbj_readwrite;
            "
          ''}
        '';
      };
    };
  };
}
