# ──────────────────────────────────────────────────────────────
# Native systemd services — Icecast, Liquidsoap, Webhook
# ──────────────────────────────────────────────────────────────
#
# Runs all three streaming services as native systemd units with
# DynamicUser isolation (Icecast, Liquidsoap) and a static
# unprivileged user (Webhook, which needs narrow sudo for
# restarting the other two).
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.streaming;

  # Bundle hooks.yaml into the Nix store
  hooksYaml = ../services/webhook/hooks.yaml;

  # Wrapper script that calls sudo systemctl restart <unit>
  restartCmd = pkgs.writeShellScriptBin "kpbj-restart" ''
    exec /run/wrappers/bin/sudo ${pkgs.systemd}/bin/systemctl "$@"
  '';

  sed = "${pkgs.gnused}/bin/sed";
in
{
  options.kpbj.streaming = {
    icecastPort = lib.mkOption {
      type = lib.types.port;
      description = "Port for Icecast to listen on.";
    };

    webhookPort = lib.mkOption {
      type = lib.types.port;
      description = "Port for webhook to listen on.";
    };

    apiBase = lib.mkOption {
      type = lib.types.str;
      description = "Base URL for the playout API (e.g. https://www.kpbj.fm/api/playout).";
    };
  };

  config = {
    # ── Icecast ─────────────────────────────────────────────────
    systemd.services.kpbj-icecast = {
      description = "KPBJ Icecast streaming server";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        RuntimeDirectory = "kpbj-icecast";
        LogsDirectory = "kpbj-icecast";

        # Copy icecast.xml template and substitute secrets + NixOS paths.
        # The '+' prefix runs ExecStartPre as root so it can read sops
        # secret files, then hands off to ExecStart which runs as DynamicUser.
        ExecStartPre = "+" + pkgs.writeShellScript "kpbj-icecast-setup" ''
          set -euo pipefail
          cfg=/run/kpbj-icecast/icecast.xml

          xml_edit() {
            local tag="$1" value="$2"
            ${sed} -i "s|<$tag>[^<]*</$tag>|<$tag>$value</$tag>|g" "$cfg"
          }

          cp ${../services/icecast/icecast.xml} "$cfg"
          chmod 644 "$cfg"

          # Secrets from sops
          xml_edit source-password "$(cat ${config.sops.secrets.icecast_password.path})"
          xml_edit relay-password  "$(cat ${config.sops.secrets.icecast_relay_password.path})"
          xml_edit admin-password  "$(cat ${config.sops.secrets.icecast_admin_password.path})"

          # NixOS paths
          xml_edit basedir    "${pkgs.icecast}/share/icecast"
          xml_edit webroot    "${pkgs.icecast}/share/icecast/web"
          xml_edit adminroot  "${pkgs.icecast}/share/icecast/admin"

          # Port and log directory
          xml_edit port   "${toString cfg.icecastPort}"
          xml_edit logdir "/var/log/kpbj-icecast"
        '';

        ExecStart = "${pkgs.icecast}/bin/icecast -c /run/kpbj-icecast/icecast.xml";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ "/var/log/kpbj-icecast" ];
      };
    };

    # ── Liquidsoap ──────────────────────────────────────────────
    systemd.services.kpbj-liquidsoap = {
      description = "KPBJ Liquidsoap audio automation";
      after = [ "kpbj-icecast.service" "network-online.target" ];
      requires = [ "kpbj-icecast.service" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        ICECAST_HOST = "127.0.0.1";
        ICECAST_PORT = toString cfg.icecastPort;
        ICECAST_MOUNT = "/stream";
        API_BASE = cfg.apiBase;
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };

      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        EnvironmentFile = [ config.sops.templates."kpbj-liquidsoap.env".path ];
        ExecStart = "${pkgs.liquidsoap}/bin/liquidsoap ${../services/liquidsoap/radio.liq}";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Webhook ─────────────────────────────────────────────────
    # Static user so we can grant narrow sudo privileges.
    users.users.kpbj-webhook = {
      isSystemUser = true;
      group = "kpbj-webhook";
      description = "KPBJ webhook service";
    };
    users.groups.kpbj-webhook = { };

    security.sudo.extraRules = [
      {
        users = [ "kpbj-webhook" ];
        commands = [
          {
            command = "${pkgs.systemd}/bin/systemctl restart kpbj-icecast.service";
            options = [ "NOPASSWD" ];
          }
          {
            command = "${pkgs.systemd}/bin/systemctl restart kpbj-liquidsoap.service";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];

    systemd.services.kpbj-webhook = {
      description = "KPBJ webhook for service restarts";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        ICECAST_SERVICE = "kpbj-icecast.service";
        LIQUIDSOAP_SERVICE = "kpbj-liquidsoap.service";
        RESTART_CMD = "${restartCmd}/bin/kpbj-restart";
      };

      serviceConfig = {
        Type = "simple";
        User = "kpbj-webhook";
        Group = "kpbj-webhook";
        EnvironmentFile = [ config.sops.templates."kpbj-webhook.env".path ];
        ExecStart = "${pkgs.webhook}/bin/webhook -hooks ${hooksYaml} -template -verbose -port ${toString cfg.webhookPort}";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening (NoNewPrivileges omitted — sudo needs setuid)
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };
  };
}
