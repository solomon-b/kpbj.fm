# ──────────────────────────────────────────────────────────────
# Dev VM — local streaming stack via QEMU
# ──────────────────────────────────────────────────────────────
#
# Runs the exact same streaming.nix + sops.nix configuration as
# staging/prod inside a lightweight QEMU VM. No Docker, no
# divergent codepaths.
#
# Usage:
#   just stream-dev-build   # Build the VM
#   just stream-dev-start   # Run the VM (port-forwards 8000, 9000)
#   just stream-dev-stop    # Kill the VM
# ──────────────────────────────────────────────────────────────
{ modulesPath, lib, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/qemu-vm.nix")
    ./common.nix
    ./streaming.nix
    ./sops.nix
  ];

  networking.hostName = "kpbj-stream-dev";
  networking.firewall.allowedTCPPorts = [ 8000 9000 ];

  kpbj.sops.secretsFile = ../secrets/dev-streaming.yaml;
  kpbj.streaming = {
    icecastPort = 8000;
    webhookPort = 9000;
    apiBase = "http://10.0.2.2:4000/api/playout"; # QEMU host gateway
    rewriteHost = "10.0.2.2"; # rewrite localhost audio URLs for VM
  };

  # Share developer's age key into VM for sops decryption
  sops.age.sshKeyPaths = lib.mkForce [ ]; # override sops.nix default (no SSH host key)
  sops.age.keyFile = "/var/lib/sops-age/keys.txt";
  virtualisation.sharedDirectories.sops-key = {
    source = "/tmp/kpbj-dev-sops-key";
    target = "/var/lib/sops-age";
  };

  virtualisation = {
    memorySize = 1024;
    cores = 2;
    graphics = false;
    forwardPorts = [
      { from = "host"; host.port = 2222; guest.port = 22; }
      { from = "host"; host.port = 8000; guest.port = 8000; }
      { from = "host"; host.port = 9000; guest.port = 9000; }
    ];
  };

}
