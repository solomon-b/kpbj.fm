# ──────────────────────────────────────────────────────────────
# Production streaming host — stream.kpbj.fm
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
  ];

  networking.hostName = "kpbj-stream-prod";

  kpbj.sops.secretsFile = ../secrets/prod-streaming.yaml;

  kpbj.streaming = {
    icecastPort = 8000;
    webhookPort = 9000;
    apiBase = "https://www.kpbj.fm/api/playout";
  };

  kpbj.syncHostEmails = {
    enable = true;
    dryRun = false;
  };

  kpbj.nginx = {
    domain = "stream.kpbj.fm";
    acmeEmail = "contact@kpbj.fm";
    icecastPort = 8000;
    webhookPort = 9000;
  };
}
