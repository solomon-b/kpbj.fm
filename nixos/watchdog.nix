# ──────────────────────────────────────────────────────────────
# Watchdog — LLM-powered log anomaly detection timer
# ──────────────────────────────────────────────────────────────
#
# Runs the watchdog bash script as a oneshot systemd service on
# a timer. Collects recent journald logs, sends them to Gemini
# for anomaly analysis, and emails alerts via msmtp when issues
# are detected. Reads GEMINI_API_KEY from its own SOPS secret
# and SMTP password from the shared web secrets.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.watchdog;

  knownIssuesFile = pkgs.writeText "kpbj-watchdog-known-issues"
    (lib.concatStringsSep "\n" cfg.knownIssues);

  watchdogScript = pkgs.writeShellApplication {
    name = "kpbj-watchdog";
    runtimeInputs = [ pkgs.curl pkgs.jq pkgs.msmtp pkgs.systemd pkgs.procps ];
    text = builtins.readFile ./scripts/watchdog.sh;
  };
in
{
  options.kpbj.watchdog = {
    enable = lib.mkEnableOption "KPBJ watchdog log monitor";

    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = "Path to the SOPS-encrypted YAML file containing gemini_api_key and smtp_password.";
    };

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*:0/15";
      description = "Systemd OnCalendar spec for how often to run the watchdog.";
    };

    lookbackMinutes = lib.mkOption {
      type = lib.types.int;
      default = 15;
      description = "How many minutes of logs to look back each run. Should match timerInterval.";
    };

    recipientEmail = lib.mkOption {
      type = lib.types.str;
      description = "Email address to send alerts to.";
    };

    environment = lib.mkOption {
      type = lib.types.str;
      description = "Environment name shown in alerts (e.g. 'prod', 'staging').";
    };

    knownIssues = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Known issues the watchdog should ignore unless the pattern changes significantly.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── SOPS secrets ──────────────────────────────────────────
    sops.secrets.gemini_api_key = {
      sopsFile = cfg.secretsFile;
    };

    # Declare smtp_password here too so watchdog.nix is
    # self-contained. SOPS-nix deduplicates declarations for
    # the same secret name, so this is safe alongside web.nix.
    sops.secrets.smtp_password = {
      sopsFile = cfg.secretsFile;
    };

    # ── SOPS templates ────────────────────────────────────────
    sops.templates."kpbj-watchdog.env" = {
      content = ''
        GEMINI_API_KEY=${config.sops.placeholder.gemini_api_key}
        WATCHDOG_ENV=${cfg.environment}
        WATCHDOG_RECIPIENT=${cfg.recipientEmail}
        WATCHDOG_INTERVAL=${toString cfg.lookbackMinutes}
        MSMTP_CONFIG=${config.sops.templates."kpbj-watchdog-msmtprc".path}
      '';
    };

    sops.templates."kpbj-watchdog-msmtprc" = {
      content = ''
        account default
        host smtp.gmail.com
        port 587
        tls on
        tls_trust_file /etc/ssl/certs/ca-certificates.crt
        auth on
        user noreply@kpbj.fm
        password ${config.sops.placeholder.smtp_password}
        from noreply@kpbj.fm
      '';
    };

    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-watchdog = {
      description = "KPBJ watchdog — LLM log anomaly detection";

      serviceConfig = {
        Type = "oneshot";

        EnvironmentFile = [ config.sops.templates."kpbj-watchdog.env".path ];
        Environment = [ "WATCHDOG_KNOWN_ISSUES_FILE=${knownIssuesFile}" ];

        ExecStart = "${watchdogScript}/bin/kpbj-watchdog";

        # Hardening
        NoNewPrivileges = true;
        ProtectHome = true;
        PrivateTmp = true;
        ProtectSystem = "full";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-watchdog = {
      description = "Run KPBJ watchdog on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        RandomizedDelaySec = "60";
        Persistent = true;
      };
    };
  };
}
