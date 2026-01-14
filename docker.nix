{ pkgs, kpbj-api }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/solomon-b/kpbj.fm";
  created = "now";
  tag = kpbj-api.version;
  # https://discourse.nixos.org/t/copy-files-into-a-docker-container-using-copytoroot/21144/5
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    pathsToLink = [
      "/bin"
      "/etc"
      "/static"
      "/migrations"
      "/tmp"
    ];
    paths = [
      pkgs.busybox
      pkgs.cacert
      pkgs.fakeNss
      pkgs.gnutar
      pkgs.gzip
      pkgs.sqlx-cli
      (pkgs.haskell.lib.justStaticExecutables kpbj-api)
      ./.
      # Create /tmp directory for temporary file operations
      (pkgs.runCommand "tmp-dir" { } "mkdir -p $out/tmp && chmod 1777 $out/tmp")
    ];
  };

  config = {
    Entrypoint = [ "/bin/sh" ];
    Cmd = [
      "-c"
      "/bin/kpbj-api"
    ];

    # Default env vars - override via fly.toml or `fly secrets set`
    Env = [
      "APP_ENVIRONMENT=Production"
      "APP_WARP_PORT=4000"
      "APP_WARP_TIMEOUT=100"
      "APP_OBSERVABILITY_VERBOSITY=Brief"
      "APP_OBSERVABILITY_EXPORTER=StdOut"
    ];
  };
}
