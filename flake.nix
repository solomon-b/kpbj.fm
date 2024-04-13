{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;

    cfg-src = {
      url = github:JonathanLorimer/cfg;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, cfg-src }:
    let
      ghcVersion = "963";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          hsPkgs = pkgs.haskell.packages.${compiler}.override {
            overrides = hfinal: hprev: {
              kpbj-backend = (hfinal.callCabal2nix "kpbj-backend" ./backend { });
              cfg = pkgs.haskell.lib.doJailbreak (hfinal.callCabal2nix "cfg" "${cfg-src}" { });
              rel8 = pkgs.haskell.lib.dontCheck hprev.rel8;
              servant-auth-server = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck hprev.servant-auth-server);
            };
          };
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskell.compiler.${compiler}
              pkgs.haskell.packages.${compiler}.haskell-language-server
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.zlib
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            kpbj-backend = hsPkgs.kpbj-backend;

            docker = import ./docker.nix {
              inherit pkgs;
              kpbj-backend = hsPkgs.kpbj-backend;
            };

            deploy = pkgs.writeShellScriptBin "deploy" ''
              nix build .#docker
              image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
              docker push $image
            '';
          };

          defaultPackage = packages.kpbj-backend;

          apps = {
            kpbj-backend = { 
              type = "app";
              program = "${self.packages.${system}.kpbj-backend}/bin/kpbj-backend";
            };

            deploy = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy; };
            default = self.apps.${system}.kpbj-backend;
          };
        });
}
