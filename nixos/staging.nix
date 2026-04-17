# ──────────────────────────────────────────────────────────────
# Staging host — staging.kpbj.fm + stream.staging.kpbj.fm
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:
{
  imports = [
    ./hardware-digitalocean.nix
    ./networking-staging.nix
    ./common.nix
    ./fail2ban.nix
    ./streaming.nix
    ./sync-host-emails.nix
    ./token-cleanup.nix
    ./episode-check.nix
    ./listener-snapshots.nix
    ./order-cleanup.nix
    ./monitoring.nix
    ./nginx.nix
    ./sops.nix
    ./pgbackrest.nix
    ./postgresql.nix
    ./web.nix
  ];

  networking.hostName = "kpbj-staging";

  # ── Intrusion prevention ───────────────────────────────────────
  kpbj.fail2ban.enable = true;

  kpbj.sops.secretsFile = ../secrets/staging-streaming.yaml;

  kpbj.streaming = {
    icecastPort = 8001;
    icecastHostname = "stream.staging.kpbj.fm";
    webhookPort = 9001;
    apiBase = "http://127.0.0.1:4000/api/playout";
    restartOnDeploy = true;
  };

  kpbj.syncHostEmails = {
    enable = true;
    dryRun = true;
  };

  kpbj.tokenCleanup = {
    enable = true;
    dryRun = false;
  };

  kpbj.episodeCheck = {
    enable = true;
    dryRun = true;
  };

  kpbj.listenerSnapshots = {
    enable = true;
  };

  kpbj.orderCleanup = {
    enable = true;
    dryRun = false;
  };

  kpbj.nginx = {
    domain = "stream.staging.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8001;
  };

  # ── Monitoring (Grafana + Loki + Promtail) ───────────────────
  kpbj.monitoring.enable = true;
  kpbj.monitoring.alerting.enable = true;
  kpbj.monitoring.alerting.environment = "staging";

  # ── Secrets for friendly-ghost ──────────────────────────────
  sops.secrets.deepseek_api_key = {
    sopsFile = ../secrets/staging-web.yaml;
  };

  sops.secrets.discord_webhook_url = {
    sopsFile = ../secrets/staging-web.yaml;
  };

  sops.templates."grafana.env" = {
    content = ''
      DISCORD_WEBHOOK_URL=${config.sops.placeholder.discord_webhook_url}
    '';
    owner = "grafana";
  };

  systemd.services.grafana.serviceConfig.EnvironmentFile =
    config.sops.templates."grafana.env".path;

  # ── friendly-ghost (LLM log anomaly detection) ──────────────
  # Override DynamicUser so the service can read sops secret files
  # under /run/secrets/ (owned by root). The old watchdog ran as root
  # for the same reason. Other hardening settings are kept.
  systemd.services.friendly-ghost.serviceConfig.DynamicUser = lib.mkForce false;

  services.friendly-ghost = {
    enable = true;
    interval = "*:0/30";

    journal = {
      units = [
        "kpbj-.*"
        "postgresql"
        "sshd"
      ];
      priority = "info";
      ignorePatterns = [
        # ── HTTP noise ──
        "HTTP/[0-9.]+ (200|301|302) "
        # ── Liquidsoap routine ──
        "Ffmpeg_decoder\\.End_of_file"
        "Finished with"
        "Prepared .* (RID|rid)"
        "Could not update timestamps for discarded samples"
        "Header missing"
        "Unsynchronized headers not handled"
        "Incorrect BOM value"
        "Error reading comment frame, skipped"
        "Unsupported MIME type"
        "Unsupported file extension"
        "video: \\{codec: mjpeg"
        "crossfade duration is longer"
        # ── PostgreSQL routine ──
        "automatic (vacuum|analyze) of table"
        "checkpoint (starting|complete)"
        "archived WAL file"
        # ── Bot scanners ──
        "\\.(env|git/config|git/HEAD|wp-admin|wp-login)"
        "androxgh0st"
        "child_process\\.execSync"
        "process\\.mainModule"
        # ── pgBackRest routine ──
        "^P00 +INFO"
        # ── Staging known issues ──
        "sync-host-emails.*dry.run"
      ];
    };

    email = {
      smtpHost = "smtp.gmail.com";
      smtpPort = 587;
      username = "noreply@kpbj.fm";
      from = "noreply@kpbj.fm";
      to = [ "ssbothwell@gmail.com" ];
      subjectPrefix = "[KPBJ staging]";
      passwordFile = config.sops.secrets.smtp_password.path;
    };

    llm = {
      enable = true;
      apiUrl = "https://api.deepseek.com/chat/completions";
      model = "deepseek-chat";
      systemPromptFile = ./scripts/friendly-ghost-prompt.txt;
      apiKeyFile = config.sops.secrets.deepseek_api_key.path;
      maxTokens = 2048;
    };
  };

  # ── pgBackRest (PG backups + WAL archiving) ──────────────────
  kpbj.pgbackrest = {
    enable = true;
    s3 = {
      bucket = "kpbj-pgbackrest";
      endpoint = "sfo3.digitaloceanspaces.com";
      region = "sfo3";
      path = "/staging";
      secretsFile = ../secrets/staging-web.yaml;
    };
  };

  # ── PostgreSQL ───────────────────────────────────────────────
  kpbj.postgresql = {
    enable = true;
    pgPackage = pkgs.postgresql_17;
    dbName = "kpbj_fm";
    dbUser = "kpbj_fm";
    # passwordFile is set by web.nix from SOPS secret
  };

  # ── Web service ──────────────────────────────────────────────
  kpbj.web = {
    enable = true;
    secretsFile = ../secrets/staging-web.yaml;
    hostname = "staging.kpbj.fm";
    environment = "Staging";
    serverName = "staging.kpbj.fm";
    domain = "staging.kpbj.fm";
    port = 4000;

    # S3 (DigitalOcean Spaces)
    bucketName = "staging-kpbj-storage";
    awsRegion = "sfo3";
    awsEndpointUrl = "https://sfo3.digitaloceanspaces.com";

    # SMTP
    smtpServer = "smtp.gmail.com";
    smtpPort = 587;
    smtpUsername = "noreply@kpbj.fm";
    smtpFromEmail = "noreply@kpbj.fm";
    smtpFromName = "KPBJ 95.9FM";

    # Webhook (co-located on same VPS)
    webhookUrl = "http://127.0.0.1:9001";

    # Stream
    streamUrl = "https://stream.staging.kpbj.fm/";
    metadataUrl = "https://stream.staging.kpbj.fm/status";
  };
}
