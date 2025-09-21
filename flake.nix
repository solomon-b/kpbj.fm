{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
    flake-utils.url = github:numtide/flake-utils;

    web-server-core = {
      url = github:solomon-b/web-server;
    };
  };

  outputs = { self, nixpkgs, flake-utils, web-server-core }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              web-server-core.overlays.web-server-core
            ];
          };
          hsPkgs = pkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              hasql = pkgs.haskell.lib.dontCheck hfinal.hasql_1_9_1_2;

              hasql-pool = pkgs.haskell.lib.dontCheck (
                pkgs.haskell.lib.dontCheck (
                  hfinal.callHackageDirect {
                    pkg = "hasql-pool";
                    ver = "1.3.0.2";
                    sha256 = "sha256-3tADBDSR7MErgVLzIZdivVqyU99/A7jsRV3qUS7wWns=";
                  } { }
                )
              );

              hasql-transaction = pkgs.haskell.lib.dontCheck (
                pkgs.haskell.lib.dontCheck (
                  hfinal.callHackageDirect {
                    pkg = "hasql-transaction";
                    ver = "1.2.0.1";
                    sha256 = "sha256-gXLDMlD6E3degEUJOtFCiZf9EAsWEBJqsOfZK54iBSA=";
                  } { }
                )
              );

              kpbj-api = hfinal.callCabal2nix "kpbj-api" ./backend { };

              text-builder = pkgs.haskell.lib.dontCheck hfinal.text-builder_1_0_0_3;

              web-server-core = web-server-core.packages.${system}.web-server-core;
            };
          };
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              pkgs.flyctl
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.hlint
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.zlib
              pkgs.zlib.dev
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            kpbj-api = hsPkgs.kpbj-api;

            docker = import ./docker.nix {
              inherit pkgs;
              kpbj-api = hsPkgs.kpbj-api;
            };

            deploy = pkgs.writeShellScriptBin "deploy" ''
              nix build .#docker
              image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
              docker push $image
            '';
          };

          defaultPackage = packages.kpbj-api;

          apps = {
            kpbj-api = {
              type = "app";
              program = "${self.packages.${system}.kpbj-api}/bin/kpbj-api";
            };

            deploy = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy; };
            default = self.apps.${system}.kpbj-api;
          };
        });
}
