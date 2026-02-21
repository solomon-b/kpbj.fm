{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
    flake-utils.url = github:numtide/flake-utils;

    web-server-core = {
      url = github:solomon-b/web-server;
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, web-server-core, sops-nix }:
    (flake-utils.lib.eachDefaultSystem
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

              sync-host-emails = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "sync-host-emails" ./jobs/sync-host-emails { });

              token-cleanup = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "token-cleanup" ./jobs/token-cleanup { });

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
            packages = p: [ p.kpbj-api p.sync-host-emails p.token-cleanup ];
            withHoogle = false;
            buildInputs = [
              pkgs.cabal-install
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.weeder
              pkgs.file
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.rclone
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.google-cloud-sdk
              pkgs.sops
              pkgs.age
              pkgs.ssh-to-age
              pkgs.nixos-rebuild
              pkgs.playwright-test
              pkgs.sloc
              pkgs.pkg-config
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            kpbj-api = hsPkgs.kpbj-api;
            sync-host-emails = hsPkgs.sync-host-emails;
            token-cleanup = hsPkgs.token-cleanup;
          };

          defaultPackage = packages.kpbj-api;

          apps = {
            kpbj-api = {
              type = "app";
              program = "${self.packages.${system}.kpbj-api}/bin/kpbj-api";
            };

            default = self.apps.${system}.kpbj-api;
          };
        }))
    //
    {
      nixosConfigurations =
        let
          sync-host-emails = self.packages.x86_64-linux.sync-host-emails;
          token-cleanup = self.packages.x86_64-linux.token-cleanup;
          kpbj-api = self.packages.x86_64-linux.kpbj-api;
        in
        {
          kpbj-stream-prod = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup kpbj-api; };
            modules = [
              sops-nix.nixosModules.sops
              ./nixos/prod.nix
            ];
          };
          kpbj-stream-staging = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup kpbj-api; };
            modules = [
              sops-nix.nixosModules.sops
              ./nixos/staging.nix
            ];
          };
          kpbj-stream-dev = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              sops-nix.nixosModules.sops
              ./nixos/dev.nix
            ];
          };
        };
    };
}
