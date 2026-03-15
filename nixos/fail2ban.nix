# ──────────────────────────────────────────────────────────────
# fail2ban — intrusion prevention
# ──────────────────────────────────────────────────────────────
#
# Watches nginx access logs and sshd journal for malicious
# activity and bans offending IPs via nftables. Repeat offenders
# get progressively longer bans.
#
# Note: www.kpbj.fm is proxied through Cloudflare, so firewall
# bans only block direct access (SSH, stream). The nginx
# real_ip_header directive ensures logs contain the true client
# IP, so attackers are still detected and blocked from all
# direct-access services.
# ──────────────────────────────────────────────────────────────
{ config, lib, pkgs, ... }:

let
  cfg = config.kpbj.fail2ban;

  # Custom filter: known-bad paths that only scanners/bots request.
  # Two hits and you're banned — legitimate users never touch these.
  badPathsFilter = pkgs.writeText "nginx-bad-paths.conf" ''
    [Definition]
    failregex = ^<HOST> - \S+ \[.*\] "(GET|POST|HEAD|PUT|DELETE|OPTIONS) /(?:wp-login|wp-admin|wp-content|wp-includes|xmlrpc\.php|\.env|\.git|\.htaccess|\.aws|phpmy|phpmyadmin|PMA|admin/config|cgi-bin|setup\.php|administrator|config\.php|actuator|api/v1/pods|solr|telescope|debug|_profiler|eval-stdin).*" \d+ \d+
    ignoreregex =
  '';
in
{
  options.kpbj.fail2ban = {
    enable = lib.mkEnableOption "KPBJ fail2ban intrusion prevention";
  };

  config = lib.mkIf cfg.enable {
    services.fail2ban = {
      enable = true;
      maxretry = 3;
      bantime = "1h";

      bantime-increment = {
        enable = true;
        maxtime = "168h"; # 1 week cap
      };

      jails = {
        # SSH brute force — uses built-in sshd filter
        sshd.settings = {
          enabled = true;
          maxretry = 3;
        };

        # Known-bad paths (wp-admin, .env, phpMyAdmin, etc.)
        # Only scanners hit these — low retry threshold
        nginx-bad-paths.settings = {
          enabled = true;
          port = "http,https";
          filter = "nginx-bad-paths";
          logpath = "/var/log/nginx/access.log";
          maxretry = 2;
          bantime = "24h";
        };

        # Too many 404s — uses built-in nginx-botsearch filter
        nginx-botsearch.settings = {
          enabled = true;
          port = "http,https";
          logpath = "/var/log/nginx/access.log";
          maxretry = 5;
        };
      };
    };

    # Install the custom filter
    environment.etc."fail2ban/filter.d/nginx-bad-paths.conf".source = badPathsFilter;
  };
}
