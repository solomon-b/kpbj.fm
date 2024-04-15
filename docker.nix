{ pkgs, kpbj-backend }:

pkgs.dockerTools.buildLayeredImage {
  name = "sbothwell/kpbj.fm";
  created = "now";
  tag = "latest";
  contents = [
    pkgs.bash
    pkgs.docker-client
  ];
  config = {
    Entrypoint = "${pkgs.bash}/bin/bash";
    Cmd = [
      "-c"
      "${pkgs.haskell.lib.justStaticExecutables kpbj-backend}/bin/kpbj-backend"
    ];
  };
}

