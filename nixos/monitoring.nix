# ──────────────────────────────────────────────────────────────
# Monitoring — Grafana + Loki + Promtail
# ──────────────────────────────────────────────────────────────
#
# Centralized log aggregation for all KPBJ services. Promtail
# scrapes journald for kpbj-* units and postgresql, ships to
# Loki. Grafana provides the query/dashboard UI.
#
# All services bind to localhost only — access via SSH tunnel:
#   ssh -N -L 3000:127.0.0.1:3000 root@<host>
# ──────────────────────────────────────────────────────────────
{ config, lib, ... }:

let
  cfg = config.kpbj.monitoring;
in
{
  options.kpbj.monitoring = {
    enable = lib.mkEnableOption "KPBJ monitoring (Grafana + Loki + Promtail)";

    grafanaPort = lib.mkOption {
      type = lib.types.port;
      default = 3000;
      description = "Port for Grafana web UI (localhost only).";
    };

    lokiPort = lib.mkOption {
      type = lib.types.port;
      default = 3100;
      description = "Port for Loki HTTP API (localhost only).";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Loki — log storage backend ─────────────────────────────
    services.loki = {
      enable = true;
      configuration = {
        auth_enabled = false;

        server = {
          http_listen_address = "127.0.0.1";
          http_listen_port = cfg.lokiPort;
          grpc_listen_address = "127.0.0.1";
          grpc_listen_port = 9095;
        };

        common = {
          path_prefix = "/var/lib/loki";
          ring = {
            instance_addr = "127.0.0.1";
            kvstore.store = "inmemory";
          };
          replication_factor = 1;
        };

        # Tell the query frontend worker to reach gRPC via
        # localhost, not the public IP the hostname resolves to.
        frontend_worker.frontend_address = "127.0.0.1:9095";

        schema_config.configs = [{
          from = "2024-01-01";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }];

        storage_config.filesystem.directory = "/var/lib/loki/chunks";

        limits_config = {
          retention_period = "720h"; # 30 days
          allow_structured_metadata = false;
        };

        compactor = {
          working_directory = "/var/lib/loki/compactor";
          delete_request_store = "filesystem";
          retention_enabled = true;
        };
      };
    };

    # ── Promtail — scrapes journald, ships to Loki ─────────────
    #
    # Disable namespace sandboxing — the NixOS module enables
    # PrivateTmp / ProtectSystem by default, which requires
    # user-namespace support.  Some VPS kernels block this,
    # causing ExecStartPre to exit 226/NAMESPACE.
    systemd.services.promtail.serviceConfig = {
      PrivateTmp = lib.mkForce false;
      ProtectSystem = lib.mkForce false;
      ProtectHome = lib.mkForce false;
      PrivateDevices = lib.mkForce false;
    };

    services.promtail = {
      enable = true;
      configuration = {
        server = {
          http_listen_address = "127.0.0.1";
          http_listen_port = 9080;
          grpc_listen_port = 0; # Disable unused gRPC server
        };

        clients = [{
          url = "http://127.0.0.1:${toString cfg.lokiPort}/loki/api/v1/push";
        }];

        scrape_configs = [{
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels.job = "systemd-journal";
          };
          relabel_configs = [{
            source_labels = [ "__journal__systemd_unit" ];
            target_label = "unit";
          }];
          pipeline_stages = [{
            match = {
              selector = ''{unit!~"kpbj-.*|postgresql.service|icecast.service"}'';
              action = "drop";
            };
          }];
        }];
      };
    };

    # ── Grafana — dashboards & query UI ────────────────────────
    services.grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
        };

        # Disable login for localhost-only access via SSH tunnel
        "auth.anonymous" = {
          enabled = true;
          org_role = "Admin";
        };
      };

      # Pre-configure Loki as a datasource
      provision.datasources.settings.datasources = [{
        name = "Loki";
        type = "loki";
        access = "proxy";
        url = "http://127.0.0.1:${toString cfg.lokiPort}";
        isDefault = true;
      }];
    };
  };
}
