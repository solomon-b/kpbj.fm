# ──────────────────────────────────────────────────────────────
# Production host — www.kpbj.fm + stream.kpbj.fm
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:
{
  imports = [
    ./hardware-digitalocean.nix
    ./networking-prod.nix
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

  networking.hostName = "kpbj-prod";

  # ── Intrusion prevention ───────────────────────────────────────
  kpbj.fail2ban.enable = true;

  kpbj.sops.secretsFile = ../secrets/prod-streaming.yaml;

  kpbj.streaming = {
    icecastPort = 8000;
    webhookPort = 9000;
    apiBase = "http://127.0.0.1:4000/api/playout";
  };

  kpbj.syncHostEmails = {
    enable = true;
    dryRun = false;
  };

  kpbj.tokenCleanup = {
    enable = true;
    dryRun = false;
  };

  kpbj.episodeCheck = {
    enable = true;
  };

  kpbj.listenerSnapshots = {
    enable = true;
  };

  kpbj.orderCleanup = {
    enable = true;
    dryRun = false;
  };

  kpbj.nginx = {
    domain = "stream.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8000;
  };

  # ── Monitoring (Grafana + Loki + Promtail) ───────────────────
  kpbj.monitoring.enable = true;

  # ── Secrets for friendly-ghost ──────────────────────────────
  sops.secrets.deepseek_api_key = {
    sopsFile = ../secrets/prod-web.yaml;
  };

  # ── friendly-ghost (LLM log anomaly detection) ──────────────
  systemd.services.friendly-ghost.serviceConfig.DynamicUser = lib.mkForce false;

  services.friendly-ghost = {
    enable = true;
    interval = "*:0/15";

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
      ];
    };

    email = {
      smtpHost = "smtp.gmail.com";
      smtpPort = 587;
      username = "noreply@kpbj.fm";
      from = "noreply@kpbj.fm";
      to = [ "ssbothwell@gmail.com" ];
      subjectPrefix = "[KPBJ prod]";
      passwordFile = config.sops.secrets.smtp_password.path;
    };

    llm = {
      enable = true;
      apiUrl = "https://api.deepseek.com/chat/completions";
      model = "deepseek-chat";
      systemPromptFile = ./scripts/friendly-ghost-prompt.txt;
      apiKeyFile = config.sops.secrets.deepseek_api_key.path;
    };
  };

  # ── pgBackRest (PG backups + WAL archiving) ──────────────────
  kpbj.pgbackrest = {
    enable = true;
    s3 = {
      bucket = "kpbj-pgbackrest";
      endpoint = "sfo3.digitaloceanspaces.com";
      region = "sfo3";
      path = "/prod";
      secretsFile = ../secrets/prod-web.yaml;
    };
  };

  # ── PostgreSQL ───────────────────────────────────────────────
  kpbj.postgresql = {
    enable = true;
    pgPackage = pkgs.postgresql_17;
    dbName = "kpbj_fm";
    dbUser = "kpbj_fm";
  };

  # ── Web service ──────────────────────────────────────────────
  # ── Bare domain → www redirect ─────────────────────────────
  services.nginx.virtualHosts."kpbj.fm" = {
    forceSSL = true;
    enableACME = true;
    globalRedirect = "www.kpbj.fm";
  };

  kpbj.web = {
    enable = true;
    secretsFile = ../secrets/prod-web.yaml;
    hostname = "www.kpbj.fm";
    environment = "Production";
    serverName = "www.kpbj.fm";
    domain = "www.kpbj.fm";
    uploadsDomain = "uploads.kpbj.fm";
    port = 4000;

    # S3 (DigitalOcean Spaces)
    bucketName = "production-kpbj-storage";
    awsRegion = "sfo3";
    awsEndpointUrl = "https://sfo3.digitaloceanspaces.com";

    # SMTP
    smtpServer = "smtp.gmail.com";
    smtpPort = 587;
    smtpUsername = "noreply@kpbj.fm";
    smtpFromEmail = "noreply@kpbj.fm";
    smtpFromName = "KPBJ 95.9FM";

    # Webhook (co-located on same VPS)
    webhookUrl = "http://127.0.0.1:9000";

    # Stream
    streamUrl = "https://stream.kpbj.fm/";
    metadataUrl = "https://stream.kpbj.fm/status";
  };
}
