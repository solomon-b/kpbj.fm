# ──────────────────────────────────────────────────────────────
# Audio normalization job — nightly systemd timer
# ──────────────────────────────────────────────────────────────
#
# Runs the audio-normalize CLI as a oneshot systemd service on a
# timer. Processes unprocessed episode audio files with two-pass
# ffmpeg loudnorm, compression, and limiting. Reads DATABASE_URL
# and S3 credentials from the existing kpbj-web.env SOPS template.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, audio-normalize, ... }:

let
  cfg = config.kpbj.audioNormalize;
  webCfg = config.kpbj.web;
in
{
  options.kpbj.audioNormalize = {
    enable = lib.mkEnableOption "KPBJ audio normalization batch job";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run in dry-run mode (list what would be processed without processing).";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 03:00:00";
      description = "Systemd calendar spec for when to run the job (default: 3 AM daily).";
    };

    maxRetries = lib.mkOption {
      type = lib.types.int;
      default = 3;
      description = "Max processing attempts per episode before giving up.";
    };

    limit = lib.mkOption {
      type = lib.types.int;
      default = 50;
      description = "Max episodes to process per run.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-audio-normalize = {
      description = "KPBJ audio normalization batch job";

      # Wait for postgresql to be available
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      # S3 environment variables (same as web service, no new ones)
      environment = {
        BUCKET_NAME = webCfg.bucketName;
        AWS_ENDPOINT_URL_S3 = webCfg.awsEndpointUrl;
        AWS_REGION = webCfg.awsRegion;
      };

      # ffmpeg for audio processing, awscli2 for S3 file operations
      path = [ pkgs.ffmpeg pkgs.awscli2 ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        # Reuse the existing kpbj-web.env which contains DATABASE_URL,
        # AWS_ACCESS_KEY_ID, and AWS_SECRET_ACCESS_KEY
        EnvironmentFile = [ config.sops.templates."kpbj-web.env".path ];

        ExecStart = "${audio-normalize}/bin/audio-normalize"
          + lib.optionalString cfg.dryRun " --dry-run"
          + " --max-retries ${toString cfg.maxRetries}"
          + " --limit ${toString cfg.limit}";

        # Generous timeout for batch processing (1 hour)
        TimeoutStartSec = "3600";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-audio-normalize = {
      description = "Run KPBJ audio normalization on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        Persistent = true;
      };
    };
  };
}
