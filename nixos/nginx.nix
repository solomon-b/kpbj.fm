# ──────────────────────────────────────────────────────────────
# Nginx reverse proxy + ACME (Let's Encrypt)
# ──────────────────────────────────────────────────────────────
#
# HTTP-01 challenge works because the stream DNS records are
# Cloudflare DNS-only (not proxied).
# ──────────────────────────────────────────────────────────────
{ config, lib, ... }:

let
  cfg = config.kpbj.nginx;
in
{
  options.kpbj.nginx = {
    domain = lib.mkOption {
      type = lib.types.str;
      description = "Domain for the streaming server (e.g. stream.kpbj.fm).";
    };

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      description = "Email for Let's Encrypt ACME registration.";
    };

    icecastPort = lib.mkOption {
      type = lib.types.port;
      description = "Local port where Icecast is listening.";
    };
  };

  config = {
    # ── ACME ──────────────────────────────────────────────────
    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    # ── Nginx ─────────────────────────────────────────────────
    services.nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts.${cfg.domain} = {
        forceSSL = true;
        enableACME = true;

        locations."= /" = {
          proxyPass = "http://127.0.0.1:${toString cfg.icecastPort}/stream";
          extraConfig = ''
            proxy_buffering off;
            proxy_read_timeout 24h;
            proxy_send_timeout 24h;
          '';
        };

        locations."= /status" = {
          proxyPass = "http://127.0.0.1:${toString cfg.icecastPort}/status-json.xsl";
        };

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.icecastPort}";
          extraConfig = ''
            proxy_buffering off;
            proxy_read_timeout 24h;
            proxy_send_timeout 24h;
          '';
        };
      };
    };

    # Nginx needs access to ACME certs
    users.users.nginx.extraGroups = [ "acme" ];
  };
}
