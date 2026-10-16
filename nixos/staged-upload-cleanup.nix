# ──────────────────────────────────────────────────────────────
# Staged upload cleanup job — systemd timer (hourly)
# ──────────────────────────────────────────────────────────────
#
# Deletes expired pending staged uploads and their files in the
# staging prefix (temp/staging/). Staged uploads expire 24 hours
# after upload. Reads DATABASE_URL and the AWS keys from the
# kpbj-web.env SOPS template. Takes the S3 settings and
# APP_ENVIRONMENT from the kpbj.web options, so they are the
# same as the web service.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, staged-upload-cleanup, ... }:

let
  cfg = config.kpbj.stagedUploadCleanup;
  webCfg = config.kpbj.web;
in
{
  options.kpbj.stagedUploadCleanup = {
    enable = lib.mkEnableOption "KPBJ staged upload cleanup job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (list the expired uploads and delete nothing).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "hourly";
      description = "Systemd calendar spec for how often to run the job.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-staged-upload-cleanup = {
      description = "KPBJ expired staged upload cleanup";

      # Wait for postgresql and the network (S3)
      after = [ "postgresql.service" "network-online.target" ];
      requires = [ "postgresql.service" ];
      wants = [ "network-online.target" ];

      environment = {
        APP_ENVIRONMENT = webCfg.environment;
        AWS_ENDPOINT_URL_S3 = webCfg.awsEndpointUrl;
        AWS_REGION = webCfg.awsRegion;
        BUCKET_NAME = webCfg.bucketName;
      };

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        # Reuse kpbj-web.env, which holds DATABASE_URL and the AWS keys
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart = "${staged-upload-cleanup}/bin/staged-upload-cleanup"
          + lib.optionalString cfg.dryRun " --dry-run";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-staged-upload-cleanup = {
      description = "Run KPBJ staged upload cleanup on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
