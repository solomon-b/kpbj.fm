# ──────────────────────────────────────────────────────────────
# Production host — www.kpbj.fm + stream.kpbj.fm
# ──────────────────────────────────────────────────────────────
{ ... }:
{
  imports = [
    ./hardware-digitalocean.nix
    ./networking-prod.nix
    ./common.nix
    ./streaming.nix
    ./sync-host-emails.nix
    ./nginx.nix
    ./sops.nix
    ./postgresql.nix
    ./web.nix
  ];

  networking.hostName = "kpbj-stream-prod";

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

  kpbj.nginx = {
    domain = "stream.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8000;
  };

  # ── PostgreSQL ───────────────────────────────────────────────
  kpbj.postgresql = {
    enable = true;
    dbName = "kpbj_fm";
    dbUser = "kpbj_fm";
  };

  # ── Web service ──────────────────────────────────────────────
  kpbj.web = {
    enable = true;
    secretsFile = ../secrets/prod-web.yaml;
    hostname = "https://www.kpbj.fm";
    environment = "Production";
    serverName = "www.kpbj.fm";
    domain = "www.kpbj.fm";
    port = 4000;

    # S3 (Tigris)
    bucketName = "production-kpbj-storage";
    awsRegion = "auto";
    awsEndpointUrl = "https://fly.storage.tigris.dev";

    # SMTP
    smtpServer = "smtp.gmail.com";
    smtpPort = 587;
    smtpUsername = "noreply@kpbj.fm";
    smtpFromEmail = "noreply@kpbj.fm";
    smtpFromName = "KPBJ 95.9FM";
    baseUrl = "https://www.kpbj.fm";

    # Webhook (co-located on same VPS)
    webhookUrl = "http://127.0.0.1:9000";

    # Stream
    streamUrl = "https://stream.kpbj.fm/";
    metadataUrl = "https://stream.kpbj.fm/status";
  };
}
