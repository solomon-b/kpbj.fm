# ──────────────────────────────────────────────────────────────
# Staging streaming host — stream.staging.kpbj.fm
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
    webhookPort = 9001;
  };
}
