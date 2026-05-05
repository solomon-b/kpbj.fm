{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.11;
    flake-utils.url = github:numtide/flake-utils;

    web-server-core = {
      url = github:solomon-b/web-server;
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    friendly-ghost = {
      url = "github:solomon-b/friendly-ghost";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, web-server-core, sops-nix, friendly-ghost }:
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

              kpbj-types = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-types" ./lib/kpbj-types { });

              kpbj-database = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-database" ./lib/kpbj-database { });

              kpbj-email = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-email" ./lib/kpbj-email { });

              stripe-http = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "stripe-http" ./lib/stripe-http { });

              easypost-http = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "easypost-http" ./lib/easypost-http { });

              kpbj-api = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-api" ./services/web { });

              sync-host-emails = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "sync-host-emails" ./jobs/sync-host-emails { });

              token-cleanup = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "token-cleanup" ./jobs/token-cleanup { });

              episode-check = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "episode-check" ./jobs/episode-check { });

              listener-snapshots = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "listener-snapshots" ./jobs/listener-snapshots { });

              ga-poller = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "ga-poller" ./jobs/ga-poller { });

              order-cleanup = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "order-cleanup" ./jobs/order-cleanup { });

              lucid-form-builder = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-form-builder" ./services/web/lib/lucid-form-builder { });

              lucid-htmx-alpine = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-htmx-alpine" ./services/web/lib/lucid-htmx-alpine { });

              lucid-tailwind = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-tailwind" ./services/web/lib/lucid-tailwind { });

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
                    rev = "c6df09aa3607452662b9f6d8713502c595df9511";
                    sha256 = "sha256-11whPVqlHH9u9qiNIx5Zi+4AP/eazxPlDyspJH+QMHY=";
                  } + "/xmlhtml-qq")
                { });

              xmlhtml-lens = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-lens"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "c6df09aa3607452662b9f6d8713502c595df9511";
                    sha256 = "sha256-11whPVqlHH9u9qiNIx5Zi+4AP/eazxPlDyspJH+QMHY=";
                  } + "/xmlhtml-lens")
                { });
            };
          };
        in
        rec {
          devShell = hsPkgs.shellFor {
            packages = p: map pkgs.haskell.lib.doCheck [ p.kpbj-types p.kpbj-database p.kpbj-email p.kpbj-api p.sync-host-emails p.token-cleanup p.episode-check p.listener-snapshots p.ga-poller ];
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
            episode-check = hsPkgs.episode-check;
            listener-snapshots = hsPkgs.listener-snapshots;
            ga-poller = hsPkgs.ga-poller;
            order-cleanup = hsPkgs.order-cleanup;
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
          episode-check = self.packages.x86_64-linux.episode-check;
          listener-snapshots = self.packages.x86_64-linux.listener-snapshots;
          ga-poller = self.packages.x86_64-linux.ga-poller;
          order-cleanup = self.packages.x86_64-linux.order-cleanup;
          kpbj-api = self.packages.x86_64-linux.kpbj-api;
        in
        {
          kpbj-prod = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup episode-check listener-snapshots ga-poller order-cleanup kpbj-api; };
            modules = [
              sops-nix.nixosModules.sops
              friendly-ghost.nixosModules.default
              ./nixos/prod.nix
            ];
          };
          kpbj-staging = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup episode-check listener-snapshots ga-poller order-cleanup kpbj-api; };
            modules = [
              sops-nix.nixosModules.sops
              friendly-ghost.nixosModules.default
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
