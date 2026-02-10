# ──────────────────────────────────────────────────────────────
# OCI container module — Icecast, Liquidsoap, Webhook
# ──────────────────────────────────────────────────────────────
#
# Podman containers managed by systemd, matching the existing
# docker-compose.yml structure. All containers join a named
# "kpbj" network so liquidsoap can resolve kpbj-icecast by
# hostname.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.streaming;

  # Bundle hooks.yaml into the Nix store
  hooksYaml = ../services/liquidsoap/hooks.yaml;
in
{
  options.kpbj.streaming = {
    imageTag = lib.mkOption {
      type = lib.types.str;
      default = "latest";
      description = "Container image tag for streaming services.";
    };

    icecastPort = lib.mkOption {
      type = lib.types.port;
      description = "Host port for Icecast (mapped to container port 8000).";
    };

    webhookPort = lib.mkOption {
      type = lib.types.port;
      description = "Host port for webhook (mapped to container port 9000).";
    };

    apiBase = lib.mkOption {
      type = lib.types.str;
      description = "Base URL for the playout API (e.g. https://www.kpbj.fm/api/playout).";
    };

    environmentFiles = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = "Paths to environment files containing secrets.";
    };
  };

  config = {
    # ── Podman network ──────────────────────────────────────────
    # Create a named network so containers can resolve each other
    # by name (replaces Docker Compose's default network).
    systemd.services.podman-network-kpbj = {
      description = "Create Podman network for KPBJ streaming";
      after = [ "podman.service" ];
      requires = [ "podman.service" ];
      before = [
        "podman-kpbj-icecast.service"
        "podman-kpbj-liquidsoap.service"
        "podman-kpbj-webhook.service"
      ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.podman}/bin/podman network create kpbj --ignore";
        ExecStop = "${pkgs.podman}/bin/podman network rm kpbj --force";
      };
    };

    # ── Containers ──────────────────────────────────────────────
    virtualisation.oci-containers = {
      backend = "podman";

      containers.kpbj-icecast = {
        image = "ghcr.io/solomon-b/kpbj-icecast:${cfg.imageTag}";
        ports = [ "127.0.0.1:${toString cfg.icecastPort}:8000" ];
        environmentFiles = cfg.environmentFiles;
        extraOptions = [ "--network=kpbj" "--health-cmd=curl -sf http://localhost:8000/status-json.xsl" "--health-interval=5s" ];
      };

      containers.kpbj-liquidsoap = {
        image = "ghcr.io/solomon-b/kpbj-liquidsoap:${cfg.imageTag}";
        dependsOn = [ "kpbj-icecast" ];
        environment = {
          ICECAST_HOST = "kpbj-icecast";
          ICECAST_PORT = "8000";
          ICECAST_MOUNT = "/stream";
          API_BASE = cfg.apiBase;
        };
        environmentFiles = cfg.environmentFiles;
        extraOptions = [ "--network=kpbj" ];
      };

      containers.kpbj-webhook = {
        image = "ghcr.io/solomon-b/kpbj-webhook:${cfg.imageTag}";
        ports = [ "127.0.0.1:${toString cfg.webhookPort}:9000" ];
        volumes = [
          "/var/run/docker.sock:/var/run/docker.sock"
          "${hooksYaml}:/config/hooks.yaml:ro"
        ];
        environment = {
          ICECAST_CONTAINER = "kpbj-icecast";
          LIQUIDSOAP_CONTAINER = "kpbj-liquidsoap";
        };
        environmentFiles = cfg.environmentFiles;
        extraOptions = [ "--network=kpbj" ];
      };
    };

    # ── Ensure containers start after the network ───────────────
    systemd.services.podman-kpbj-icecast.after = [ "podman-network-kpbj.service" ];
    systemd.services.podman-kpbj-icecast.requires = [ "podman-network-kpbj.service" ];
    systemd.services.podman-kpbj-liquidsoap.after = [ "podman-network-kpbj.service" ];
    systemd.services.podman-kpbj-liquidsoap.requires = [ "podman-network-kpbj.service" ];
    systemd.services.podman-kpbj-webhook.after = [ "podman-network-kpbj.service" ];
    systemd.services.podman-kpbj-webhook.requires = [ "podman-network-kpbj.service" ];
  };
}
