# ──────────────────────────────────────────────────────────────
# PostgreSQL — local database for the KPBJ web service
# ──────────────────────────────────────────────────────────────
#
# Runs PostgreSQL 17 with a dedicated database and user.
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
  };

  config = lib.mkIf cfg.enable {
    # ── PostgreSQL service ───────────────────────────────────────
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_17;

      ensureDatabases = [ cfg.dbName ];
      ensureUsers = [
        {
          name = cfg.dbUser;
          ensureDBOwnership = true;
        }
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
          PASSWORD=$(cat ${cfg.passwordFile})
          ${pkgs.util-linux}/bin/runuser -u postgres -- \
            ${config.services.postgresql.package}/bin/psql -c \
            "ALTER USER ${cfg.dbUser} PASSWORD '$PASSWORD';"
        '';
      };
    };
  };
}
