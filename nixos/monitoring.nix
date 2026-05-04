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
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.monitoring;

  # Helper to build a Grafana alert rule from a Loki query.
  mkAlertRule = { uid, title, expr, description ? "", filter ? "" }: {
    inherit uid title;
    annotations = { inherit description filter; };
    condition = "B";
    data = [
      {
        refId = "A";
        relativeTimeRange = { from = 300; to = 0; };
        datasourceUid = "loki";
        model = {
          inherit expr;
          refId = "A";
          queryType = "instant";
        };
      }
      {
        refId = "B";
        relativeTimeRange = { from = 300; to = 0; };
        datasourceUid = "__expr__";
        model = {
          type = "threshold";
          expression = "A";
          conditions = [{
            evaluator = { type = "gt"; params = [ 0 ]; };
          }];
        };
      }
    ];
    noDataState = "OK";
    execErrState = "Error";
    "for" = "0s";
  };

  # Helper to build a Grafana alert rule from a Prometheus query with a
  # configurable threshold. Defaults: "gt" comparator, 5-minute sustain.
  mkPromAlertRule =
    { uid
    , title
    , expr
    , threshold
    , description ? ""
    , op ? "gt"
    , forDuration ? "5m"
    ,
    }: {
      inherit uid title;
      annotations = { inherit description; };
      condition = "B";
      data = [
        {
          refId = "A";
          relativeTimeRange = { from = 300; to = 0; };
          datasourceUid = "prometheus";
          model = {
            inherit expr;
            refId = "A";
            instant = true;
            range = false;
          };
        }
        {
          refId = "B";
          relativeTimeRange = { from = 300; to = 0; };
          datasourceUid = "__expr__";
          model = {
            type = "threshold";
            expression = "A";
            conditions = [{
              evaluator = { type = op; params = [ threshold ]; };
            }];
          };
        }
      ];
      noDataState = "OK";
      execErrState = "Error";
      "for" = forDuration;
    };
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

    alerting = {
      enable = lib.mkEnableOption "KPBJ alerting (Discord notifications)";

      environment = lib.mkOption {
        type = lib.types.str;
        description = "Environment label for alert messages (e.g. 'prod', 'staging').";
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
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
                selector = ''{unit!~"kpbj-.*|postgresql.service|icecast.service|friendly-ghost.service"}'';
                action = "drop";
              };
            }];
          }];
        };
      };

      # ── Prometheus — metrics collection ─────────────────────────
      services.prometheus = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9090;
        retentionTime = "30d";

        exporters.node = {
          enable = true;
          listenAddress = "127.0.0.1";
          enabledCollectors = [ "systemd" ];
        };

        scrapeConfigs = [{
          job_name = "node";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }];
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

        # Pre-configure dashboards
        provision.dashboards.settings.providers = [{
          name = "KPBJ";
          options.path = ./dashboards;
        }];

        # Pre-configure datasources
        provision.datasources.settings.datasources = [
          {
            name = "Loki";
            uid = "loki";
            type = "loki";
            access = "proxy";
            url = "http://127.0.0.1:${toString cfg.lokiPort}";
            isDefault = true;
          }
          {
            name = "Prometheus";
            uid = "prometheus";
            type = "prometheus";
            access = "proxy";
            url = "http://127.0.0.1:9090";
          }
        ];
      };
    }

    # ── Alerting — Discord notifications ───────────────────────
    (lib.mkIf cfg.alerting.enable {
      services.grafana.provision.alerting = {
        contactPoints.settings = {
          apiVersion = 1;
          contactPoints = [{
            orgId = 1;
            name = "Discord";
            receivers = [{
              uid = "discord-kpbj";
              type = "discord";
              settings = {
                url = "\${DISCORD_WEBHOOK_URL}";
                title = "[${cfg.alerting.environment}] {{ .CommonLabels.alertname }}";
                message = builtins.concatStringsSep "\n" [
                  "**Environment:** ${cfg.alerting.environment}"
                  "**Alert:** {{ .CommonLabels.alertname }}"
                  ""
                  "{{ range .Alerts }}"
                  "**Status:** {{ .Status }}"
                  "**Unit:** {{ .Labels.unit }}"
                  "{{ if .Annotations.description }}**Details:** {{ .Annotations.description }}{{ end }}"
                  "[View Logs](http://localhost:3000/d/service-logs/Service-Logs?var-unit={{ .Labels.unit }}&var-filter={{ .Annotations.filter | urlquery }}&from={{ .StartsAt.UnixMilli }}&to=now)"
                  "{{ end }}"
                ];
              };
            }];
          }];
        };

        policies.settings = {
          apiVersion = 1;
          policies = [{
            orgId = 1;
            receiver = "Discord";
            group_by = [ "grafana_folder" "alertname" ];
            group_wait = "30s";
            group_interval = "5m";
            repeat_interval = "4h";
          }];
        };

        rules.settings = {
          apiVersion = 1;
          groups = [
            {
              orgId = 1;
              name = "KPBJ Service Alerts";
              folder = "KPBJ";
              interval = "5m";
              rules = [
                (mkAlertRule {
                  uid = "http-5xx";
                  title = "HTTP 5xx Errors";
                  description = "The web service returned HTTP 5xx server errors in the last 5 minutes. Check web service logs for stack traces.";
                  filter = ''"statusCode":5[0-9]{2}'';
                  expr = ''count_over_time({unit="kpbj-web.service"} |~ "\"statusCode\":5[0-9]{2}" [5m])'';
                })
                (mkAlertRule {
                  uid = "app-errors";
                  title = "Application Errors";
                  description = "The web service logged application-level errors in the last 5 minutes. Check web service logs for details.";
                  filter = ''"level":"error"'';
                  expr = ''count_over_time({unit="kpbj-web.service"} |~ "\"level\":\"error\"" [5m])'';
                })
                (mkAlertRule {
                  uid = "db-errors";
                  title = "Database Errors";
                  description = "PostgreSQL logged FATAL or PANIC errors in the last 5 minutes. Database may be unreachable or corrupted.";
                  filter = "FATAL|PANIC";
                  expr = ''count_over_time({unit="postgresql.service"} |~ "FATAL|PANIC" [5m])'';
                })
                (mkAlertRule {
                  uid = "liquidsoap-errors";
                  title = "Liquidsoap Errors";
                  description = "Liquidsoap logged critical errors (severity 0 or 1) in the last 5 minutes. Audio stream may be interrupted.";
                  filter = ''\[.*:[01]\]'';
                  expr = ''count_over_time({unit=~"kpbj-liquidsoap.*"} |~ "\\[.*:[01]\\]" !~ "crossfade duration is longer" [5m])'';
                })
                (mkAlertRule {
                  uid = "icecast-errors";
                  title = "Icecast Errors";
                  description = "Icecast logged ERROR or FATAL messages in the last 5 minutes. Listener stream may be down.";
                  filter = "ERROR|FATAL";
                  expr = ''count_over_time({unit=~"kpbj-icecast.*|icecast.service"} |~ "ERROR|FATAL" [5m])'';
                })
                (mkAlertRule {
                  uid = "webhook-errors";
                  title = "Webhook Errors";
                  description = "The webhook service logged errors in the last 5 minutes. Dashboard commands (restart, force-play, skip) may not be working.";
                  filter = ''ERROR|FATAL|"level":"error"'';
                  expr = ''count_over_time({unit=~"kpbj-webhook.*"} |~ "ERROR|FATAL|\"level\":\"error\"" [5m])'';
                })
                (mkAlertRule {
                  uid = "batch-job-errors";
                  title = "Batch Job Errors";
                  description = "A scheduled batch job (token-cleanup, sync-host-emails, episode-check, listener-snapshots, ga-poller, or order-cleanup) logged errors in the last 5 minutes.";
                  filter = ''ERROR|FATAL|error:|"level":"error"'';
                  expr = ''count_over_time({unit=~"kpbj-(token-cleanup|sync-host-emails|episode-check|listener-snapshots|ga-poller|order-cleanup).*"} |~ "ERROR|FATAL|error:|\"level\":\"error\"" [5m])'';
                })
                (mkAlertRule {
                  uid = "friendly-ghost-errors";
                  title = "friendly-ghost Errors";
                  description = "The friendly-ghost log monitor logged errors or failed in the last 30 minutes. Log anomaly detection may not be running.";
                  filter = "error:|Failed with result|status=1";
                  expr = ''count_over_time({unit="friendly-ghost.service"} |~ "error:|Failed with result|status=1" [30m])'';
                })
              ];
            }
            {
              orgId = 1;
              name = "KPBJ Resource Alerts";
              folder = "KPBJ";
              interval = "1m";
              rules = [
                (mkPromAlertRule {
                  uid = "high-memory";
                  title = "High Memory Usage";
                  description = "Host memory usage above 85% for 5 minutes. Consider restarting leaky services or upgrading the droplet.";
                  expr = ''(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100'';
                  threshold = 85;
                  forDuration = "5m";
                })
                (mkPromAlertRule {
                  uid = "low-disk-warning";
                  title = "Low Disk Space (Warning)";
                  description = "Root filesystem has less than 15% free space. Clean up logs, old backups, or expand the volume.";
                  expr = ''(node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay"} / node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay"}) * 100'';
                  threshold = 15;
                  op = "lt";
                  forDuration = "5m";
                })
                (mkPromAlertRule {
                  uid = "low-disk-critical";
                  title = "Low Disk Space (Critical)";
                  description = "Root filesystem has less than 5% free space. Services may fail to write — act immediately.";
                  expr = ''(node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay"} / node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay"}) * 100'';
                  threshold = 5;
                  op = "lt";
                  forDuration = "2m";
                })
                (mkPromAlertRule {
                  uid = "high-cpu";
                  title = "High CPU Usage";
                  description = "CPU usage above 90% for 10 minutes. Check for runaway processes or unexpected traffic.";
                  expr = ''100 - (avg(rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100)'';
                  threshold = 90;
                  forDuration = "10m";
                })
                (mkPromAlertRule {
                  uid = "high-load";
                  title = "High Load Average";
                  description = "5-minute load average exceeds 2x the core count for 10 minutes. System is saturated.";
                  expr = ''node_load5 / count(count(node_cpu_seconds_total) by (cpu))'';
                  threshold = 2;
                  forDuration = "10m";
                })
              ];
            }
          ];
        };
      };
    })
  ]);
}
