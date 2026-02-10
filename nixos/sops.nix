# ──────────────────────────────────────────────────────────────
# SOPS secrets — decrypts streaming secrets at deploy time
# ──────────────────────────────────────────────────────────────
#
# Declares all streaming secrets and renders a combined env file
# that gets mounted into Podman containers. The SOPS-encrypted
# YAML in the repo is decrypted on the VPS using the host's SSH
# key (converted to age).
# ──────────────────────────────────────────────────────────────
{ config, lib, ... }:
let
  cfg = config.kpbj.sops;
in
{
  options.kpbj.sops = {
    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = "Path to the SOPS-encrypted YAML file for this environment.";
    };
  };

  config = {
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    sops.defaultSopsFile = cfg.secretsFile;

    sops.secrets.icecast_admin_password = { };
    sops.secrets.icecast_relay_password = { };
    sops.secrets.icecast_password = { };
    sops.secrets.webhook_secret = { };
    sops.secrets.playout_secret = { };

    sops.templates."kpbj-stream.env" = {
      content = ''
        ICECAST_ADMIN_PASSWORD=${config.sops.placeholder.icecast_admin_password}
        ICECAST_RELAY_PASSWORD=${config.sops.placeholder.icecast_relay_password}
        ICECAST_PASSWORD=${config.sops.placeholder.icecast_password}
        WEBHOOK_SECRET=${config.sops.placeholder.webhook_secret}
        PLAYOUT_SECRET=${config.sops.placeholder.playout_secret}
      '';
      restartUnits = [
        "podman-kpbj-icecast.service"
        "podman-kpbj-liquidsoap.service"
        "podman-kpbj-webhook.service"
      ];
    };

    # Wire the rendered env file into the streaming module
    kpbj.streaming.environmentFiles = [ config.sops.templates."kpbj-stream.env".path ];
  };
}
