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

              hasql-pool = pkgs.haskell.lib.dontCheck hfinal.hasql-pool_1_3_0_1;

              hasql-transaction = pkgs.haskell.lib.dontCheck hfinal.hasql-transaction_1_2_0_1;

              kpbj-api = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-api" ./. { });

              text-builder = pkgs.haskell.lib.dontCheck hfinal.text-builder_1_0_0_3;

              # Use tmp-postgres from master to match cabal.project pin
              tmp-postgres = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "tmp-postgres"
                (pkgs.fetchgit {
                  url = "https://github.com/jfischoff/tmp-postgres.git";
                  rev = "refs/heads/master";
                  sha256 = "sha256-dE1OQN7I4Lxy6RBdLCvm75Z9D/Hu+9G4ejV2pEtvL1A=";
                })
                { });

              web-server-core = web-server-core.packages.${system}.web-server-core;
            };
          };
        in
        rec {
          devShell = hsPkgs.shellFor {
            packages = p: [ p.kpbj-api ];
            withHoogle = false;
            buildInputs = [
              pkgs.cabal-install
              pkgs.flyctl
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.weeder
              pkgs.file
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.pkg-config
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
