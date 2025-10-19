{ pkgs, kpbj-api }:

pkgs.dockerTools.buildImage {
  name = "sbothwell/kpbj.fm";
  created = "now";
  tag = kpbj-api.version;
  # https://discourse.nixos.org/t/copy-files-into-a-docker-container-using-copytoroot/21144/5
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    pathsToLink = [ "/bin" "/static" "/migrations" ];
    paths = [ pkgs.sqlx-cli (pkgs.haskell.lib.justStaticExecutables kpbj-api) ./. ];
  };

  config = {
    Entrypoint = [ "${pkgs.bash}/bin/bash" ];
    Cmd = [
      "-c"
      "/bin/kpbj-api"
    ];

    Env = [
      "APP_ENVIRONMENT=Development"
      "APP_POSTGRES_DB=kpbj"
      "APP_POSTGRES_HOST=transfigured-night"
      "APP_POSTGRES_PASSWORD=kpbj"
      "APP_POSTGRES_PORT=5432"
      "APP_POSTGRES_USER=kpbj"
      "APP_WARP_PORT=3000"
      "APP_WARP_SERVERNAME=kpbj.fm"
      "APP_WARP_TIMEOUT=100"
      "APP_OBSERVABILITY_VERBOSITY=Quiet"
      "APP_OBSERVABILITY_EXPORTER=StdOut"
    ];
  };
}
