# ──────────────────────────────────────────────────────────────
# Web service — Haskell/Servant application
# ──────────────────────────────────────────────────────────────
#
# Runs the kpbj-api binary as a native systemd service with
# DynamicUser isolation. Runs sqlx migrations before start.
# Declares SOPS secrets and renders an env file for the service.
# Adds Nginx vhosts for the web domain and uploads subdomain.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, kpbj-api, ... }:

let
  cfg = config.kpbj.web;
  pgCfg = config.kpbj.postgresql;

  migrationsDir = ../services/web/migrations;
in
{
  options.kpbj.web = {
    enable = lib.mkEnableOption "KPBJ web service";

    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = "Path to the SOPS-encrypted YAML file for web service secrets.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 4000;
      description = "Port for the web service to listen on.";
    };

    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Full URL hostname (e.g. https://staging.kpbj.fm).";
    };

    environment = lib.mkOption {
      type = lib.types.str;
      description = "Application environment (Staging or Production).";
    };

    serverName = lib.mkOption {
      type = lib.types.str;
      description = "Warp server name (e.g. staging.kpbj.fm).";
    };

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Domain for Nginx vhost (e.g. staging.kpbj.fm).";
    };

    # S3 storage
    bucketName = lib.mkOption {
      type = lib.types.str;
      description = "S3 bucket name for media storage.";
    };

    awsRegion = lib.mkOption {
      type = lib.types.str;
      default = "auto";
      description = "AWS/S3 region.";
    };

    awsEndpointUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://fly.storage.tigris.dev";
      description = "S3-compatible endpoint URL.";
    };

    # SMTP
    smtpServer = lib.mkOption {
      type = lib.types.str;
      default = "smtp.gmail.com";
      description = "SMTP server hostname.";
    };

    smtpPort = lib.mkOption {
      type = lib.types.int;
      default = 587;
      description = "SMTP server port.";
    };

    smtpUsername = lib.mkOption {
      type = lib.types.str;
      description = "SMTP authentication username.";
    };

    smtpFromEmail = lib.mkOption {
      type = lib.types.str;
      description = "From email address for outgoing mail.";
    };

    smtpFromName = lib.mkOption {
      type = lib.types.str;
      default = "KPBJ 95.9FM";
      description = "From display name for outgoing mail.";
    };

    baseUrl = lib.mkOption {
      type = lib.types.str;
      description = "Base URL for email links (e.g. https://staging.kpbj.fm).";
    };

    webhookUrl = lib.mkOption {
      type = lib.types.str;
      description = "URL of the webhook listener for service restarts.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── SOPS secrets (web-specific, from separate secrets file) ──
    sops.secrets.db_password = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" "kpbj-postgres-setup.service" ];
    };
    sops.secrets.smtp_password = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" ];
    };
    sops.secrets.aws_access_key_id = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" ];
    };
    sops.secrets.aws_secret_access_key = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" ];
    };
    sops.secrets.google_analytics_gtag = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" ];
    };

    # ── SOPS template (env file with secrets) ────────────────────
    # playout_secret and webhook_secret come from the streaming
    # secrets file (already declared in sops.nix).
    sops.templates."kpbj-web.env" = {
      content = ''
        APP_POSTGRES_PASSWORD=${config.sops.placeholder.db_password}
        APP_SMTP_PASSWORD=${config.sops.placeholder.smtp_password}
        AWS_ACCESS_KEY_ID=${config.sops.placeholder.aws_access_key_id}
        AWS_SECRET_ACCESS_KEY=${config.sops.placeholder.aws_secret_access_key}
        APP_GOOGLE_ANALYTICS_GTAG=${config.sops.placeholder.google_analytics_gtag}
        PLAYOUT_SECRET=${config.sops.placeholder.playout_secret}
        WEBHOOK_SECRET=${config.sops.placeholder.webhook_secret}
        DATABASE_URL=postgres://${pgCfg.dbUser}:${config.sops.placeholder.db_password}@127.0.0.1:5432/${pgCfg.dbName}
      '';
      restartUnits = [ "kpbj-web.service" ];
    };

    # ── Wire passwordFile into postgresql module ─────────────────
    kpbj.postgresql.passwordFile = config.sops.secrets.db_password.path;

    # ── Systemd service ──────────────────────────────────────────
    systemd.services.kpbj-web = {
      description = "KPBJ web service";
      after = [ "kpbj-postgres-setup.service" "network-online.target" ];
      requires = [ "kpbj-postgres-setup.service" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        APP_ENVIRONMENT = cfg.environment;
        APP_HOSTNAME = cfg.hostname;
        APP_WARP_PORT = toString cfg.port;
        APP_WARP_SERVERNAME = cfg.serverName;
        APP_WARP_TIMEOUT = "100";
        APP_OBSERVABILITY_EXPORTER = "StdOut";
        APP_OBSERVABILITY_VERBOSITY = "Brief";

        # Database (password comes via EnvironmentFile)
        APP_POSTGRES_HOST = "127.0.0.1";
        APP_POSTGRES_PORT = "5432";
        APP_POSTGRES_DB = pgCfg.dbName;
        APP_POSTGRES_USER = pgCfg.dbUser;

        # SMTP (password comes via EnvironmentFile)
        APP_SMTP_SERVER = cfg.smtpServer;
        APP_SMTP_PORT = toString cfg.smtpPort;
        APP_SMTP_USERNAME = cfg.smtpUsername;
        APP_SMTP_FROM_EMAIL = cfg.smtpFromEmail;
        APP_SMTP_FROM_NAME = cfg.smtpFromName;
        APP_BASE_URL = cfg.baseUrl;

        # S3 (credentials come via EnvironmentFile)
        AWS_ENDPOINT_URL_S3 = cfg.awsEndpointUrl;
        AWS_REGION = cfg.awsRegion;
        BUCKET_NAME = cfg.bucketName;

        # Webhook
        WEBHOOK_URL = cfg.webhookUrl;

        # TLS certificates for outbound HTTPS
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

        # Timezone data
        TZDIR = "${pkgs.tzdata}/share/zoneinfo";
      };

      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        # Run migrations before starting the app
        ExecStartPre = pkgs.writeShellScript "kpbj-web-migrate" ''
          set -euo pipefail
          ${pkgs.sqlx-cli}/bin/sqlx migrate run --source ${migrationsDir}
        '';

        ExecStart = "${kpbj-api}/bin/kpbj-api";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Nginx vhosts ─────────────────────────────────────────────
    services.nginx.virtualHosts = {
      ${cfg.domain} = {
        forceSSL = true;
        enableACME = true;

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 500M;
          '';
        };
      };

      "uploads.${cfg.domain}" = {
        forceSSL = true;
        enableACME = true;

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 500M;
          '';
        };
      };
    };
  };
}
