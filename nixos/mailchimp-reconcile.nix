# ──────────────────────────────────────────────────────────────
# Mailchimp reconcile — systemd timer + bootstrap unit
# ──────────────────────────────────────────────────────────────
#
# Runs the kpbj-mailchimp-reconcile CLI as a oneshot systemd
# service on a daily timer. Diffs Mailchimp's audience against
# the local newsletter_subscribers table and reconciles any
# drift (push PG-only rows, delete locally for MC-side
# unsubscribes/bounces, log warnings for MC-only signups).
#
# Reads DATABASE_URL, MAILCHIMP_API_KEY, and
# MAILCHIMP_AUDIENCE_ID from the kpbj-mailchimp-reconcile.env
# SOPS template (rendered in web.nix).
#
# A sibling `kpbj-mailchimp-reconcile-bootstrap` oneshot service
# (no timer, manualStart) runs the binary with `--bootstrap` for
# the one-time post-deploy import of all subscribed Mailchimp
# members into Postgres:
#
#   sudo systemctl start kpbj-mailchimp-reconcile-bootstrap
#
# Both units share DynamicUser + EnvironmentFile, so the
# bootstrap path doesn't need a manual `sudo -u` invocation
# (which would fail since DynamicUser users don't exist
# outside an active activation).
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, mailchimp-reconcile, ... }:

let
  cfg = config.kpbj.mailchimpReconcile;
in
{
  options.kpbj.mailchimpReconcile = {
    enable = lib.mkEnableOption "KPBJ Mailchimp reconcile job";

    timerInterval = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 02:30:00";
      description = "Systemd calendar spec (default: daily at 02:30 UTC, after ga-poller's 01:30).";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Systemd service ─────────────────────────────────────────
    systemd.services.kpbj-mailchimp-reconcile = {
      description = "KPBJ Mailchimp ↔ Postgres reconcile";

      after = [ "postgresql.service" "network-online.target" ];
      requires = [ "postgresql.service" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        EnvironmentFile = [ config.sops.templates."kpbj-mailchimp-reconcile.env".path ];

        ExecStart = "${mailchimp-reconcile}/bin/mailchimp-reconcile";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    # ── Systemd timer ───────────────────────────────────────────
    systemd.timers.kpbj-mailchimp-reconcile = {
      description = "Run KPBJ Mailchimp reconcile on a schedule";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.timerInterval;
        AccuracySec = "1m";
        Persistent = true;
      };
    };

    # ── Bootstrap service (manual start only, no timer) ─────────
    # Run once after first deploy to import existing Mailchimp
    # subscribers into Postgres:
    #   sudo systemctl start kpbj-mailchimp-reconcile-bootstrap
    systemd.services.kpbj-mailchimp-reconcile-bootstrap = {
      description = "KPBJ Mailchimp → Postgres one-time bootstrap";

      after = [ "postgresql.service" "network-online.target" ];
      requires = [ "postgresql.service" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;

        EnvironmentFile = [ config.sops.templates."kpbj-mailchimp-reconcile.env".path ];

        ExecStart = "${mailchimp-reconcile}/bin/mailchimp-reconcile --bootstrap";

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      };
    };
  };
}
