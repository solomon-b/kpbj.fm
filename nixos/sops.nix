# ──────────────────────────────────────────────────────────────
# SOPS secrets — decrypts streaming secrets at deploy time
# ──────────────────────────────────────────────────────────────
#
# Declares all streaming secrets and renders per-service env
# files. Icecast reads secrets directly from sops secret file
# paths (via ExecStartPre sed substitution). The SOPS-encrypted
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

    # ── Individual secrets ──────────────────────────────────────
    # Icecast reads these directly via config.sops.secrets.*.path
    sops.secrets.icecast_admin_password = {
      restartUnits = [ "kpbj-icecast.service" ];
    };
    sops.secrets.icecast_relay_password = {
      restartUnits = [ "kpbj-icecast.service" ];
    };
    sops.secrets.icecast_password = {
      restartUnits = [ "kpbj-icecast.service" ];
    };
    sops.secrets.webhook_secret = { };
    sops.secrets.playout_secret = { };

    # ── Per-service env files ───────────────────────────────────
    sops.templates."kpbj-liquidsoap.env" = {
      content = ''
        ICECAST_PASSWORD=${config.sops.placeholder.icecast_password}
        PLAYOUT_SECRET=${config.sops.placeholder.playout_secret}
      '';
      restartUnits = [ "kpbj-liquidsoap.service" ];
    };

    sops.templates."kpbj-webhook.env" = {
      content = ''
        WEBHOOK_SECRET=${config.sops.placeholder.webhook_secret}
      '';
      restartUnits = [ "kpbj-webhook.service" ];
    };
  };
}
