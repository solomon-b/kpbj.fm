# ──────────────────────────────────────────────────────────────
# Staging host — staging.kpbj.fm + stream.staging.kpbj.fm
# ──────────────────────────────────────────────────────────────
{ pkgs, ... }:
{
  imports = [
    ./hardware-digitalocean.nix
    ./networking-staging.nix
    ./common.nix
    ./streaming.nix
    ./sync-host-emails.nix
    ./token-cleanup.nix
    ./episode-check.nix
    ./monitoring.nix
    ./nginx.nix
    ./sops.nix
    ./pgbackrest.nix
    ./postgresql.nix
    ./web.nix
    ./watchdog.nix
  ];

  networking.hostName = "kpbj-staging";

  kpbj.sops.secretsFile = ../secrets/staging-streaming.yaml;

  kpbj.streaming = {
    icecastPort = 8001;
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

  kpbj.nginx = {
    domain = "stream.staging.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8001;
  };

  # ── Monitoring (Grafana + Loki + Promtail) ───────────────────
  kpbj.monitoring.enable = true;

  # ── Watchdog (LLM log anomaly detection) ────────────────────
  kpbj.watchdog = {
    enable = true;
    secretsFile = ../secrets/staging-web.yaml;
    recipientEmail = "ssbothwell@gmail.com";
    environment = "staging";
    timerInterval = "*:0/30";
    lookbackMinutes = 30;
    knownIssues = [
      "sync-host-emails running in dry-run mode on staging with out-of-sync membership — intentional, staging uses dry-run by design"
    ];
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
    hostname = "https://staging.kpbj.fm";
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
