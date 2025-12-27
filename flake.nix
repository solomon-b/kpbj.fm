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

              # rel8 1.7.0.0 for hasql 1.9 compatibility
              rel8 = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "rel8";
                  ver = "1.7.0.0";
                  sha256 = "sha256-B6I+y19vWCc0xh6tzCKLzHo7TB+91E7aZHPepm23/jI=";
                }
                { });

              # Use tmp-postgres from master to match cabal.project pin
              tmp-postgres = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "tmp-postgres"
                (pkgs.fetchgit {
                  url = "https://github.com/jfischoff/tmp-postgres.git";
                  rev = "refs/heads/master";
                  sha256 = "sha256-dE1OQN7I4Lxy6RBdLCvm75Z9D/Hu+9G4ejV2pEtvL1A=";
                })
                { });

              web-server-core = web-server-core.packages.${system}.web-server-core;

              # xmlhtml packages from web-server repo (same source as
              # web-server-core, but these packages aren't currently in the
              # flake output)
              xmlhtml-qq = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-qq"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "16bcc47190f10bb5cea486092d833c1c63fcf783";
                    sha256 = "sha256-LLPOmBR39+OhY7sYZ78C9R/AYnaqE91+yFWfZyKHKFY=";
                  } + "/xmlhtml-qq")
                { });

              xmlhtml-lens = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-lens"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "16bcc47190f10bb5cea486092d833c1c63fcf783";
                    sha256 = "sha256-LLPOmBR39+OhY7sYZ78C9R/AYnaqE91+yFWfZyKHKFY=";
                  } + "/xmlhtml-lens")
                { });
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
              pkgs.sloc
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

            publish = pkgs.writeShellScriptBin "publish" ''
              nix build .#docker
              image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
              docker push $image
              # Also tag and push as latest
              docker tag $image ghcr.io/solomon-b/kpbj.fm:latest
              docker push ghcr.io/solomon-b/kpbj.fm:latest
            '';
          };

          defaultPackage = packages.kpbj-api;

          apps = {
            kpbj-api = {
              type = "app";
              program = "${self.packages.${system}.kpbj-api}/bin/kpbj-api";
            };

            publish = flake-utils.lib.mkApp { drv = self.packages.${system}.publish; };
            default = self.apps.${system}.kpbj-api;
          };
        });
}
