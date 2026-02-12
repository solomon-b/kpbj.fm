# ──────────────────────────────────────────────────────────────
# Staging host — staging.kpbj.fm + stream.staging.kpbj.fm
# ──────────────────────────────────────────────────────────────
{ ... }:
{
  imports = [
    ./hardware-digitalocean.nix
    ./networking-staging.nix
    ./common.nix
    ./streaming.nix
    ./sync-host-emails.nix
    ./nginx.nix
    ./sops.nix
    ./postgresql.nix
    ./web.nix
  ];

  networking.hostName = "kpbj-stream-staging";

  kpbj.sops.secretsFile = ../secrets/staging-streaming.yaml;

  kpbj.streaming = {
    icecastPort = 8001;
    webhookPort = 9001;
    apiBase = "https://staging.kpbj.fm/api/playout";
  };

  kpbj.syncHostEmails = {
    enable = true;
    dryRun = true;
  };

  kpbj.nginx = {
    domain = "stream.staging.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8001;
  };

  # ── PostgreSQL ───────────────────────────────────────────────
  kpbj.postgresql = {
    enable = true;
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

    # S3 (Tigris)
    bucketName = "staging-kpbj-storage";
    awsRegion = "auto";
    awsEndpointUrl = "https://fly.storage.tigris.dev";

    # SMTP
    smtpServer = "smtp.gmail.com";
    smtpPort = 587;
    smtpUsername = "noreply@kpbj.fm";
    smtpFromEmail = "noreply@kpbj.fm";
    smtpFromName = "KPBJ 95.9FM";
    baseUrl = "https://staging.kpbj.fm";

    # Webhook (co-located on same VPS)
    webhookUrl = "http://127.0.0.1:9001";
  };
}
