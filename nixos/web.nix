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

    enableSSL = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable ACME certificates and force HTTPS for web vhosts.";
    };

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

    uploadsDomain = lib.mkOption {
      type = lib.types.str;
      default = "uploads.${cfg.domain}";
      description = "Domain for the uploads Nginx vhost (e.g. uploads.kpbj.fm).";
    };

    # S3 storage
    bucketName = lib.mkOption {
      type = lib.types.str;
      description = "S3 bucket name for media storage.";
    };

    awsRegion = lib.mkOption {
      type = lib.types.str;
      description = "AWS/S3 region (e.g. us-east-1, sfo3, auto).";
    };

    awsEndpointUrl = lib.mkOption {
      type = lib.types.str;
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

    streamUrl = lib.mkOption {
      type = lib.types.str;
      description = "URL of the Icecast audio stream (e.g. https://stream.kpbj.fm/).";
    };

    metadataUrl = lib.mkOption {
      type = lib.types.str;
      description = "URL of the Icecast metadata/status endpoint (e.g. https://stream.kpbj.fm/status).";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── SOPS secrets (web-specific, from separate secrets file) ──
    sops.secrets.db_password = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-web.service" "kpbj-postgres-setup.service" ];
    };
    sops.secrets.db_readonly_password = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-postgres-setup.service" ];
    };
    sops.secrets.db_readwrite_password = {
      sopsFile = cfg.secretsFile;
      restartUnits = [ "kpbj-postgres-setup.service" ];
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
    kpbj.postgresql.readonlyPasswordFile = config.sops.secrets.db_readonly_password.path;
    kpbj.postgresql.readwritePasswordFile = config.sops.secrets.db_readwrite_password.path;

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
        APP_WARP_REQUEST_TIMEOUT = "600";
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

        # Stream
        STREAM_URL = cfg.streamUrl;
        METADATA_URL = cfg.metadataUrl;

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

    # ── Nginx rate limiting ──────────────────────────────────────
    # Use CF-Connecting-IP to get the real client IP behind Cloudflare.
    # Without this, $binary_remote_addr is a Cloudflare edge IP and all
    # users behind the same POP share a single rate-limit bucket.
    services.nginx.appendHttpConfig = ''
      set_real_ip_from 173.245.48.0/20;
      set_real_ip_from 103.21.244.0/22;
      set_real_ip_from 103.22.200.0/22;
      set_real_ip_from 103.31.4.0/22;
      set_real_ip_from 141.101.64.0/18;
      set_real_ip_from 108.162.192.0/18;
      set_real_ip_from 190.93.240.0/20;
      set_real_ip_from 188.114.96.0/20;
      set_real_ip_from 197.234.240.0/22;
      set_real_ip_from 198.41.128.0/17;
      set_real_ip_from 162.158.0.0/15;
      set_real_ip_from 104.16.0.0/13;
      set_real_ip_from 104.24.0.0/14;
      set_real_ip_from 172.64.0.0/13;
      set_real_ip_from 131.0.72.0/22;
      set_real_ip_from 2400:cb00::/32;
      set_real_ip_from 2606:4700::/32;
      set_real_ip_from 2803:f800::/32;
      set_real_ip_from 2405:b500::/32;
      set_real_ip_from 2405:8100::/32;
      set_real_ip_from 2a06:98c0::/29;
      set_real_ip_from 2c0f:f248::/32;
      real_ip_header CF-Connecting-IP;

      limit_req_zone $binary_remote_addr zone=auth:10m rate=5r/m;
    '';

    # ── Nginx vhosts ─────────────────────────────────────────────
    services.nginx.virtualHosts = {
      ${cfg.domain} = {
        forceSSL = cfg.enableSSL;
        enableACME = cfg.enableSSL;

        # Rate-limited auth endpoints
        locations."/user/login" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 64k;
            limit_req zone=auth burst=3 nodelay;
            limit_req_status 429;
          '';
        };

        locations."/user/register" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 64k;
            limit_req zone=auth burst=3 nodelay;
            limit_req_status 429;
          '';
        };

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 500M;
          '';
        };
      };

      ${cfg.uploadsDomain} = {
        forceSSL = cfg.enableSSL;
        enableACME = cfg.enableSSL;

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          extraConfig = ''
            proxy_buffering off;
            client_max_body_size 500M;
            proxy_read_timeout 600;
            proxy_send_timeout 600;
            client_body_timeout 600;
          '';
        };
      };
    };
  };
}
