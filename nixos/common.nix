# ──────────────────────────────────────────────────────────────
# Shared base configuration for all KPBJ NixOS hosts
# ──────────────────────────────────────────────────────────────
{ pkgs, ... }:
{
  system.stateVersion = "25.05";

  # ── SSH ──────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };

  # ── Authorized keys ─────────────────────────────────────────
  # Matches ssh_public_keys in terraform/variables.tf
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp solomon@nightshade"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o solomon@lorean"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHWnMYdY2wfu05WThiGKlNK8aCX3HmNyQds8MOoSM+v solomon@voice-of-evening"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHcci4M3kg0q13zdlzHJNnFmmNYDUaDMAIHlNS0YFVl/ github-actions-staging-deploy"
  ];

  # ── Firewall ────────────────────────────────────────────────
  # Defense-in-depth alongside DigitalOcean firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 80 443 ];
  };

  # ── Nix ─────────────────────────────────────────────────────
  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # ── Packages ────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    curl
    htop
    vim
  ];
}
