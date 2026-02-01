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
          amazonkaSrc = pkgs.fetchgit {
            url = "https://github.com/brendanhay/amazonka.git";
            rev = "a7d699be1076e2aad05a1930ca3937ffea954ad8";
            sha256 = "sha256-cCRhHH/IgM7tPy8rXHTSRec1zxohO8NWxSVZEG1OjQw=";
          };
          hsPkgs = pkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              amazonka = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka" (amazonkaSrc + "/lib/amazonka") { });

              amazonka-core = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-core" (amazonkaSrc + "/lib/amazonka-core") { });

              amazonka-s3 = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-s3" (amazonkaSrc + "/lib/services/amazonka-s3") { });

              amazonka-sso = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-sso" (amazonkaSrc + "/lib/services/amazonka-sso") { });

              amazonka-sts = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-sts" (amazonkaSrc + "/lib/services/amazonka-sts") { });

              hasql = pkgs.haskell.lib.dontCheck hfinal.hasql_1_9_1_2;

              hasql-pool = pkgs.haskell.lib.dontCheck hfinal.hasql-pool_1_3_0_1;

              hasql-transaction = pkgs.haskell.lib.dontCheck hfinal.hasql-transaction_1_2_0_1;

              kpbj-api = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-api" ./services/web { });

              lucid-form-builder = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-form-builder" ./services/web/lib/lucid-form-builder { });

              lucid-htmx-alpine = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-htmx-alpine" ./services/web/lib/lucid-htmx-alpine { });

              lucid-tailwind = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-tailwind" ./services/web/lib/lucid-tailwind { });

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
                  rev = "7f2467a6d6d5f6db7eed59919a6773fe006cf22b";
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

            # Docker image with configurable tag
            # For local builds: nix build .#docker (uses cabal version)
            # For CI builds: IMAGE_TAG=sha-xxx nix build .#docker --impure (uses IMAGE_TAG)
            docker =
              let
                # Use IMAGE_TAG env var if set (requires --impure), otherwise use cabal version
                envTag = builtins.getEnv "IMAGE_TAG";
                tag = if envTag != "" then envTag else hsPkgs.kpbj-api.version;
              in
              import ./services/web/docker.nix {
                inherit pkgs;
                kpbj-api = hsPkgs.kpbj-api;
                imageTag = tag;
              };

            # Publish script that builds and pushes docker image
            # Usage: IMAGE_TAG=0.3.2 nix run .#publish --impure
            # Set PUSH_LATEST=true to also push :latest tag
            publish = pkgs.writeShellScriptBin "publish" ''
              set -euo pipefail

              if [ -z "''${IMAGE_TAG:-}" ]; then
                echo "ERROR: IMAGE_TAG environment variable is required"
                echo "Usage: IMAGE_TAG=0.3.2 nix run .#publish --impure"
                exit 1
              fi

              echo "Building docker image with tag: $IMAGE_TAG"
              nix build .#docker --impure

              image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
              echo "Loaded image: $image"
              docker push "$image"

              if [ "''${PUSH_LATEST:-false}" = "true" ]; then
                echo "Also tagging and pushing as :latest"
                docker tag "$image" ghcr.io/solomon-b/kpbj.fm:latest
                docker push ghcr.io/solomon-b/kpbj.fm:latest
              fi

              echo "Done! Pushed: $image"
            '';

            # weeder-check = pkgs.stdenv.mkDerivation {
            #   name = "weeder-check";
            #   src = ./.;
            #   doCheck = true;
            #   nativeBuildInputs = [ hsPkgs.weeder hsPkgs.kpbj-api ];
            #   checkPhase = ''
            #       weeder
            #     '';
            # };
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
